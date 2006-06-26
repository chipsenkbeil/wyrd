(*  Wyrd -- a curses-based front-end for Remind
 *  Copyright (C) 2005, 2006 Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 2,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at 
 *  <pelzlpj@eecs.umich.edu>.
 *)

(* remind.ml
 * functions for interfacing with 'remind(1)' *)


exception Occurrence_not_found

open Utility


(* A record for an individual timed reminder, as read from
 * the output of 'remind -s'. *)
type timed_rem_t = {
   tr_start      : float;
   tr_end        : float;
   tr_msg        : string;
   tr_filename   : string;
   tr_linenum    : string;
   tr_has_weight : bool
}


(* A record for an individual untimed reminder, as read from
 * the output of 'remind -s'. *)
type untimed_rem_t = {
   ur_start      : float;
   ur_msg        : string;
   ur_filename   : string;
   ur_linenum    : string;
   ur_has_weight : bool
}


(* Storage for a three-month window of reminders and
 * the calendar for the current month.
 * Makes it possible to handle edge effects of moving
 * from month to month without constantly calling 
 * rem(1) and regenerating calendar layouts. 
 *
 * Timed reminders are stored in an array of lists,
 * with each element of the array representing a different
 * indentation level on the timetable.  Untimed reminders
 * have no indentation, so they are simply stored in
 * lists. *)
type three_month_rem_t = {
   curr_timestamp : float;
   prev_timed     : timed_rem_t list array;
   curr_timed     : timed_rem_t list array;
   next_timed     : timed_rem_t list array;
   all_timed      : timed_rem_t list array;
   prev_untimed   : untimed_rem_t list;
   curr_untimed   : untimed_rem_t list;
   next_untimed   : untimed_rem_t list;
   all_untimed    : untimed_rem_t list;
   curr_counts    : int array;
   curr_cal       : Cal.t;
   remind_error   : string
}


(* Get the starting timestamp and time record for a given month *)
let month_start_of_tm tm =
   let month_start_tm = {empty_tm with
      Unix.tm_mon   = tm.Unix.tm_mon;
      Unix.tm_year  = tm.Unix.tm_year;
   } in
   Unix.mktime month_start_tm



(* Process information about a timed reminder, create a new reminder
 * record for it, and store it in the timed reminders array at the
 * proper indentation level.
 *
 * The indentation levels are determined by maintaining an array that indicates
 * which levels have been used for each hourly timeslot.  As each reminder is
 * processed, we look into the array to find the smallest indentation level
 * available.  Complexity is approximately
 * (number of reminders) * (average reminder duration).  Since indentation is
 * determined a month at a time, there may be some minor discrepancies at
 * month borders. *)
let process_timed tm duration_s month_start_ts indentations partial_trem 
timed =
   let (f_rem_ts, _) = Unix.mktime tm in
   let duration =
      if duration_s = "*" then 1.0
      else float_of_string duration_s
   in
   (* compute the indentation level *)
   (* top_index and bottom_index provide the range of row indices into
    * array indentations that are covered by this reminder *)
   let top_index = 
      try int_of_float ((f_rem_ts -. month_start_ts) /. 3600.0)
      with _ -> 0
   in
   let bottom_index = 
      try
         let real_bottom_index =
            let shift = (f_rem_ts +. (duration *. 60.0) -. month_start_ts) /. 3600.0 in
            (* catch the edge effects when reminders end on hour boundaries *)
            if shift = float_of_int (int_of_float shift) then
               pred (int_of_float shift)
            else
               int_of_float shift
         in
         (* the bottom index could flow off of this month, in which case
          * we truncate and hope everything works out *)
         if real_bottom_index > pred (Array.length indentations) then
            pred (Array.length indentations)
         else
            real_bottom_index
      with _ -> top_index
   in
   (* locate the smallest free indentation level *)
   let rec find_indent level =
      if level < Array.length indentations.(0) then begin
         let collision = ref false in
         for i = top_index to bottom_index do
            if indentations.(i).(level) then
               collision := true
            else
               ()
         done;
         if !collision then
            find_indent (succ level)
         else begin
            for i = top_index to bottom_index do
               indentations.(i).(level) <- true
            done;
            level
         end
      end else
         (* give up and default to maximum indentation *)
         pred (Array.length indentations.(0))
   in
   let indent = find_indent 0 in 
   let trem = {partial_trem with
      tr_start      = f_rem_ts;
      tr_end        = f_rem_ts +. (duration *. 60.);
   } in
   timed.(indent) <- trem :: timed.(indent)



(* Obtain two lists of reminders for the month of the timestamp argument.
 * The first list is for timed reminders, the second is for untimed. 
 * The timed reminders list also provides the indentation levels that
 * draw_timed should use to render each reminder. 
 *
 * The indentation levels are determined by maintaining an array that indicates
 * which levels have been used for each hourly timeslot.  As each reminder is
 * processed, we look into the array to find the smallest indentation level
 * available.  Complexity is approximately
 * (number of reminders) * (average reminder duration).  Since indentation is
 * determined a month at a time, there may be some minor discrepancies at
 * month borders. *)
let month_reminders timestamp =
   let comment_regex = Str.regexp "^#.*fileinfo \\([^ ]+\\) \\(.*\\)$" in
   let rem_regex = Str.regexp "\\([^ ]+\\) [^ ]+ \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)$" in
   let nodisplay_regex = Str.regexp_case_fold ".*nodisplay" in
   let noweight_regex = Str.regexp_case_fold ".*noweight" in
   let tm = Unix.localtime timestamp in
   let rem_date_str = (string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let (remind_channel, remind_in_channel, remind_err_channel) = 
      Unix.open_process_full (!Rcfile.remind_command ^ " -s -l -g -b2 " ^ 
      !Rcfile.reminders_file ^ " " ^ rem_date_str) [||] 
   in
   (* check for Remind errors *)
   let remind_err =
      try input_line remind_err_channel
      with End_of_file -> ""
   in
   let num_indentations = 4 in
   let indentations = Array.make_matrix (24 * 31) num_indentations false in
   let (month_start_ts, _) = month_start_of_tm tm in
   let timed = Array.make num_indentations [] in
   let rec build_lists untimed =
      try
         let line = input_line remind_channel in
         if Str.string_match comment_regex line 0 then begin
            let line_num_s = Str.matched_group 1 line
            and filename   = Str.matched_group 2 line in
            let rem_line   = input_line remind_channel in
            if Str.string_match rem_regex rem_line 0 then begin
               let date_s     = Str.matched_group 1 rem_line
               and tag        = Str.matched_group 2 rem_line
               and duration_s = Str.matched_group 3 rem_line
               and min_s      = Str.matched_group 4 rem_line
               and msg        = Str.matched_group 5 rem_line in
               (* further subdivide the date string *)
               let date_arr = Array.of_list (Str.split (Str.regexp "/") date_s) in
               let year     = int_of_string date_arr.(0)
               and month    = int_of_string date_arr.(1)
               and day      = int_of_string date_arr.(2) in
               let temp = {empty_tm with
                  Unix.tm_mday  = day;
                  Unix.tm_mon   = pred month;
                  Unix.tm_year  = year - 1900;
               } in
               (* check whether this reminder is tagged 'nodisplay' *)
               if (Str.string_match nodisplay_regex tag 0) then
                  (* skip this reminder due to a 'nodisplay' tag *)
                  build_lists untimed
               else begin
                  let has_weight = not (Str.string_match noweight_regex tag 0) in
                  if min_s = "*" then
                     (* if minutes are not provided, this must be an untimed reminder *)
                     let (f_rem_ts, _) = Unix.mktime temp in
                     let urem = {
                        ur_start      = f_rem_ts;
                        ur_msg        = msg;
                        ur_filename   = filename;
                        ur_linenum    = line_num_s;
                        ur_has_weight = has_weight
                     } in
                     build_lists (urem :: untimed)
                  else begin
                     (* if minutes are provided, this must be a timed reminder *)
                     let temp_with_min = {temp with Unix.tm_min = int_of_string min_s} in
                     let partial_trem = {
                        tr_start      = 0.0;  (* still needs to be filled in *)
                        tr_end        = 0.0;  (* still needs to be filled in *)
                        tr_msg        = msg;
                        tr_filename   = filename;
                        tr_linenum    = line_num_s;
                        tr_has_weight = has_weight
                     } in
                     process_timed temp_with_min duration_s month_start_ts indentations
                     partial_trem timed;
                     build_lists untimed
                  end
               end
            end else
               (* if there was no rem_regex match, continue with next line *)
               build_lists untimed
         end else
            (* if there was no comment_regex match, continue with next line *)
            build_lists untimed
      with
      | End_of_file ->
           let _ = Unix.close_process_full (remind_channel, 
              remind_in_channel, remind_err_channel) 
           in
           for i = 0 to pred (Array.length timed) do
              timed.(i) <- List.rev timed.(i)
           done;
           (remind_err, timed, List.rev untimed)
      | _ ->
           (* if there's an error in regexp matching or string coersion,
            * just drop that reminder and go to the next line *)
           build_lists untimed
   in
   build_lists []


(* generate a count of how many reminders fall on any given day of
 * the month *)
let count_reminders month_start_tm timed untimed =
   let rem_counts = Array.make 31 0 in
   let count_rems start has_weight =
      let tm = Unix.localtime start in
      if has_weight && tm.Unix.tm_year = month_start_tm.Unix.tm_year &&
      tm.Unix.tm_mon = month_start_tm.Unix.tm_mon then
         let day = pred tm.Unix.tm_mday in
         rem_counts.(day) <- succ rem_counts.(day)
      else
         ()
   in
   let count_timed rem =
      count_rems rem.tr_start rem.tr_has_weight
   in
   let count_untimed rem =
      count_rems rem.ur_start rem.ur_has_weight
   in
   Array.iter (List.iter count_timed) timed;
   List.iter count_untimed untimed;
   rem_counts


(* generate a count of how many hours of reminders one has on
 * any given day of the month.  Note: some minor errors can
 * occur during DST, but this is too small to worry about. *)
let count_busy_hours month_start_tm timed untimed =
   let hour_counts = Array.make 31 0.0 in
   let last_day = 
      let temp = {month_start_tm with Unix.tm_mday = 32} in
      let (_, nextmonth) = Unix.mktime temp in
      32 - nextmonth.Unix.tm_mday
   in
   let count_hours start stop has_weight =
      if has_weight then
         for day = 1 to last_day do
            let day_tm = {month_start_tm with Unix.tm_mday = day} in
            let (day_ts, _) = Unix.mktime day_tm in
            if day_ts >= start then
               if stop > day_ts +. 86400. then
                  hour_counts.(pred day) <- hour_counts.(pred day) +. 24.
               else if stop > day_ts then
                  hour_counts.(pred day) <- hour_counts.(pred day) +.
                  ((stop -. day_ts) /. 3600.)
               else
                  ()
            else if day_ts +. 86400. > start then
               if stop > day_ts +. 86400. then
                  hour_counts.(pred day) <- hour_counts.(pred day) +.
                  24. -. ((start -. day_ts) /. 3600.)
               else
                  hour_counts.(pred day) <- hour_counts.(pred day) +.
                  ((stop -. start) /. 3600.)
            else
               ()
         done
      else
         ()
   in
   let count_timed rem =
      count_hours rem.tr_start rem.tr_end rem.tr_has_weight
   in
   let count_untimed rem =
      let stop = rem.ur_start +. (!Rcfile.untimed_duration *. 60.) in
      count_hours rem.ur_start stop rem.ur_has_weight
   in
   Array.iter (List.iter count_timed) timed;
   List.iter count_untimed untimed;
   Array.map int_of_float hour_counts


(* determine the busy-ness level for each day in the month *)
let count_busy month_tm timed untimed =
   let month_start_tm = {
      month_tm with Unix.tm_sec  = 0;
                    Unix.tm_min  = 0;
                    Unix.tm_hour = 0;
                    Unix.tm_mday = 1
   } in
   match !Rcfile.busy_algorithm with
   |1 -> count_reminders month_start_tm timed untimed
   |2 -> count_busy_hours month_start_tm timed untimed
   |_ -> Rcfile.config_failwith "busy_algorithm must be either 1 or 2"


(* comparison functions for sorting reminders chronologically *)
let cmp_timed rem_a rem_b =
   compare rem_a.tr_start rem_b.tr_start

let cmp_untimed rem_a rem_b =
   compare rem_a.ur_start rem_b.ur_start


(* take an array of timed reminder lists and merge them into
 * a single list sorted by starting timestamp *)
let merge_timed timed =
   let all_rem = ref [] in
   for i = 0 to pred (Array.length timed) do
      all_rem := List.rev_append timed.(i) !all_rem
   done;
   List.fast_sort cmp_timed !all_rem


(* same thing as List.append (or @), but tail-recursive *)
let safe_append a b = List.rev_append (List.rev a) b


(* initialize a new three-month reminder record *)
let create_three_month timestamp =
   let month_start_tm = {
      Unix.localtime timestamp with Unix.tm_sec  = 0;
                                    Unix.tm_min  = 0;
                                    Unix.tm_hour = 0;
                                    Unix.tm_mday = 1
   } in
   let (curr_ts, _) = Unix.mktime month_start_tm in
   let temp_prev = {
      month_start_tm with Unix.tm_mon = pred month_start_tm.Unix.tm_mon
   } in
   let temp_next = {
      month_start_tm with Unix.tm_mon = succ month_start_tm.Unix.tm_mon
   } in
   let (prev_ts, _) = Unix.mktime temp_prev
   and (next_ts, _) = Unix.mktime temp_next in
   let (pre, pt, pu) = month_reminders prev_ts in
   let (cre, ct, cu) = month_reminders curr_ts in
   let (nre, nt, nu) = month_reminders next_ts in 
   let at = Array.make (Array.length pt) [] in
   for i = 0 to pred (Array.length at) do
      at.(i) <- safe_append pt.(i) (safe_append ct.(i) nt.(i))
   done;
   let err_str = 
      if String.length pre > 0 then
         pre
      else if String.length cre > 0 then
         cre
      else
         nre
   in
   let au = safe_append pu (safe_append cu nu) in {
      curr_timestamp = curr_ts;
      prev_timed     = pt;
      curr_timed     = ct;
      next_timed     = nt;
      all_timed      = at;
      prev_untimed   = pu;
      curr_untimed   = cu;
      next_untimed   = nu;
      all_untimed    = au;
      curr_counts    = count_busy month_start_tm at au;
      curr_cal       = Cal.make curr_ts !Rcfile.week_starts_monday;
      remind_error   = err_str   
   }



(* Update a three-month reminders record for the next month *)
let next_month reminders =
   let curr_tm = Unix.localtime reminders.curr_timestamp in
   let temp1 = {
      curr_tm with Unix.tm_mon = succ curr_tm.Unix.tm_mon
   } in
   let temp2 = {
      curr_tm with Unix.tm_mon = curr_tm.Unix.tm_mon + 2
   } in
   let (new_curr_timestamp, temp1) = Unix.mktime temp1 in
   let (next_timestamp, temp2)     = Unix.mktime temp2 in
   let (re, t, u) = month_reminders next_timestamp in 
   let at = Array.make (Array.length t) [] in
   for i = 0 to pred (Array.length t) do
      at.(i) <- safe_append reminders.curr_timed.(i) 
                (safe_append reminders.next_timed.(i) t.(i))
   done;
   let au = safe_append reminders.curr_untimed (safe_append reminders.next_untimed u) in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = reminders.curr_timed;
      curr_timed     = reminders.next_timed;
      next_timed     = t;
      all_timed      = at;
      prev_untimed   = reminders.curr_untimed;
      curr_untimed   = reminders.next_untimed;
      next_untimed   = u;
      all_untimed    = au;
      curr_counts    = count_busy temp1 at au;
      curr_cal       = Cal.make new_curr_timestamp !Rcfile.week_starts_monday;
      remind_error   = re
   }


(* Update a three-month reminders record for the previous month *)
let prev_month reminders =
   let curr_tm = Unix.localtime reminders.curr_timestamp in
   let temp1 = {
      curr_tm with Unix.tm_mon = pred curr_tm.Unix.tm_mon
   } in
   let temp2 = {
      curr_tm with Unix.tm_mon = curr_tm.Unix.tm_mon - 2
   } in
   let (new_curr_timestamp, temp1) = Unix.mktime temp1 in
   let (prev_timestamp, temp2)     = Unix.mktime temp2 in
   let (re, t, u) = month_reminders prev_timestamp in
   let at = Array.make (Array.length t) [] in
   for i = 0 to pred (Array.length t) do
      at.(i) <- safe_append t.(i) 
                (safe_append reminders.prev_timed.(i) reminders.curr_timed.(i))
   done;
   let au = safe_append u (safe_append reminders.prev_untimed reminders.curr_untimed) in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = t;
      curr_timed     = reminders.prev_timed;
      next_timed     = reminders.curr_timed;
      all_timed      = at;
      prev_untimed   = u;
      curr_untimed   = reminders.prev_untimed;
      next_untimed   = reminders.curr_untimed;
      all_untimed    = au;
      curr_counts    = count_busy temp1 at au;
      curr_cal       = Cal.make new_curr_timestamp !Rcfile.week_starts_monday;
      remind_error   = re
   }


(* Return a new reminders record centered on the current timestamp,
 * doing as little work as possible. *)
let update_reminders rem timestamp =
   let tm     = Unix.localtime timestamp
   and rem_tm = Unix.localtime rem.curr_timestamp in
   if tm.Unix.tm_year = rem_tm.Unix.tm_year &&
      tm.Unix.tm_mon  = rem_tm.Unix.tm_mon then
      rem
   else
      let temp1 = {
         rem_tm with Unix.tm_mon = pred rem_tm.Unix.tm_mon
      } in
      let temp2 = {
         rem_tm with Unix.tm_mon = succ rem_tm.Unix.tm_mon
      } in
      let (_, prev_tm) = Unix.mktime temp1 in
      let (_, next_tm) = Unix.mktime temp2 in
      if tm.Unix.tm_year = prev_tm.Unix.tm_year &&
         tm.Unix.tm_mon  = prev_tm.Unix.tm_mon then
         prev_month rem
      else if tm.Unix.tm_year = next_tm.Unix.tm_year &&
         tm.Unix.tm_mon = next_tm.Unix.tm_mon then
         next_month rem
      else
         create_three_month timestamp



(* Look at all 'next' reminders after the given timestamp.  Search
 * through the list for the first occurrence of the search regexp, and return
 * a timestamp for that date.
 * This calls 'remind -n' twice--once for the current day, once for the next day.
 * The second call is necessary because reminders falling on the current day
 * but before the current timestamp will effectively suppress later recurrences
 * of that reminder. 
 *
 * We also have to make a separate check that the matched reminder is not tagged
 * with 'nodisplay'; since these reminders don't show up on the display, Wyrd
 * should not be able to match them. *)
let find_next msg_regex timestamp =
   let rem_regex = 
      Str.regexp "^\\([^ ]+\\)/\\([^ ]+\\)/\\([^ ]+\\) [^ ]+ \\([^ ]+\\) [^ ]+ \\([^ ]+\\) \\([^ ]+.*\\)$"
   in
   let nodisplay_regex = Str.regexp_case_fold ".*nodisplay" in
   let tm1 = Unix.localtime timestamp in
   let temp = {tm1 with Unix.tm_mday = succ tm1.Unix.tm_mday} in     (* add 24 hours *)
   let (_, tm2) = Unix.mktime temp in
   let rem_date_str1 = (string_of_tm_mon tm1.Unix.tm_mon) ^ " " ^ 
                       (string_of_int tm1.Unix.tm_mday) ^ " " ^
                       (string_of_int (tm1.Unix.tm_year + 1900)) in
   let rem_date_str2 = (string_of_tm_mon tm2.Unix.tm_mon) ^ " " ^ 
                       (string_of_int tm2.Unix.tm_mday) ^ " " ^
                       (string_of_int (tm2.Unix.tm_year + 1900)) in
   let (remind_channel, remind_in_channel, remind_err_channel) = 
      Unix.open_process_full (!Rcfile.remind_command ^ " -n -s -b1 " ^ 
      !Rcfile.reminders_file ^ " " ^ rem_date_str1 ^ " > " ^ Rcfile.tmpfile ^ " && " ^
      !Rcfile.remind_command ^ " -n -s -b1 " ^ !Rcfile.reminders_file ^ " " ^ rem_date_str2 ^ " | cat " ^ 
      Rcfile.tmpfile ^ " - | sort") [||]
   in
   let rec check_messages () =
      try
         let line = input_line remind_channel in
         if Str.string_match rem_regex line 0 then begin
            (* go here if this line is a timed reminder *)
            let year  = int_of_string (Str.matched_group 1 line)
            and month = int_of_string (Str.matched_group 2 line)
            and day   = int_of_string (Str.matched_group 3 line)
            and tag   = Str.matched_group 4 line
            and min_s = Str.matched_group 5 line
            and msg   = Str.matched_group 6 line in
            let temp = {empty_tm with
               Unix.tm_min   = if min_s = "*" then 0 else (int_of_string min_s);
               Unix.tm_mday  = day;
               Unix.tm_mon   = pred month;
               Unix.tm_year  = year - 1900;
            } in
            let (ts, _) = Unix.mktime temp in
            if ts > timestamp then
               begin try
                  let _ = Str.search_forward msg_regex msg 0 in
                  (* only return the match if this value is not tagged 'nodisplay' *)
                  if not (Str.string_match nodisplay_regex tag 0) then
                     let _ = Unix.close_process_full (remind_channel,
                        remind_in_channel, remind_err_channel)
                     in
                     ts
                  else
                     check_messages ()
               with Not_found ->
                  check_messages ()
               end
            else begin
               check_messages ()
            end
         end else
            (* if there was no regexp match, continue with next line *)
            check_messages ()
      with
      | End_of_file ->
         let _ = Unix.close_process_full (remind_channel, 
            remind_in_channel, remind_err_channel) 
         in
         raise Occurrence_not_found
      | Failure s ->
        (* if there's an error in string coersion, just drop that reminder and go
         * to the next line *)
        check_messages ()
   in
   check_messages ()


(* get a list of the main remfile and all INCLUDEd reminder files *)
let get_included_remfiles () =   
   let main_remfile = Utility.expand_file !Rcfile.reminders_file in
   let remfile_channel = open_in main_remfile in
   let include_regex = Str.regexp_case_fold "^[ \t]*include[ \t]+\\([^ \t]+\\)" in
   let rec build_filelist files =
      try
         let line = input_line remfile_channel in
         if Str.string_match include_regex line 0 then
            let new_file = Str.matched_group 1 line in
            build_filelist (new_file :: files)
         else
            build_filelist files
      with End_of_file ->
         close_in remfile_channel;
         List.rev files
   in
   build_filelist [main_remfile]


(* arch-tag: DO_NOT_CHANGE_6bb48a1c-2b0c-4254-ba3a-ee9b48007169 *)
