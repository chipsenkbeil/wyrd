(*  Wyrd -- a curses-based front-end for Remind
 *  Copyright (C) 2005 Paul Pelzl
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


exception String_of_tm_mon_failure of string
exception String_of_tm_wday_failure of string
exception Occurrence_not_found


type cal_t = {
   title    : string;
   weekdays : string;
   days     : string list
}


(* Storage for a three-month window of reminders and
 * the calendar for the current month.
 * Makes it possible to handle edge effects of moving
 * from month to month without constantly calling 
 * rem(1) and cal(1). *)
type three_month_rem_t = {
   curr_timestamp : float;
   prev_timed     : (float * float * string * string * string) list;
   curr_timed     : (float * float * string * string * string) list;
   next_timed     : (float * float * string * string * string) list;
   all_timed      : (float * float * string * string * string) list;
   prev_untimed   : (float * string * string * string) list;
   curr_untimed   : (float * string * string * string) list;
   next_untimed   : (float * string * string * string) list;
   all_untimed    : (float * string * string * string) list;
   curr_counts    : int array;
   curr_cal       : cal_t
}


let string_of_tm_mon i =
   match i with
   | 0 -> "Jan"
   | 1 -> "Feb"
   | 2 -> "Mar"
   | 3 -> "Apr"
   | 4 -> "May"
   | 5 -> "Jun"
   | 6 -> "Jul"
   | 7 -> "Aug"
   | 8 -> "Sep"
   | 9 -> "Oct"
   |10 -> "Nov"
   |11 -> "Dec"
   | x -> raise (String_of_tm_mon_failure ("unknown month " ^ (string_of_int x)))

let full_string_of_tm_mon i =
   match i with
   | 0 -> "January"
   | 1 -> "February"
   | 2 -> "March"
   | 3 -> "April"
   | 4 -> "May"
   | 5 -> "June"
   | 6 -> "Jul"
   | 7 -> "August"
   | 8 -> "September"
   | 9 -> "October"
   |10 -> "November"
   |11 -> "December"
   | x -> raise (String_of_tm_mon_failure ("unknown month " ^ (string_of_int x)))

let string_of_tm_wday i =
   match i with
   | 0 -> "Sun"
   | 1 -> "Mon"
   | 2 -> "Tuey"
   | 3 -> "Wed"
   | 4 -> "Thu"
   | 5 -> "Fri"
   | 6 -> "Sat"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))

let full_string_of_tm_wday i =
   match i with
   | 0 -> "Sunday"
   | 1 -> "Monday"
   | 2 -> "Tuesday"
   | 3 -> "Wednesday"
   | 4 -> "Thursday"
   | 5 -> "Friday"
   | 6 -> "Saturday"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))


(* Obtain two lists of reminders for the month of the timestamp argument.
 * The first list is for timed reminders, the second is for untimed. *)
let month_reminders timestamp =
   let comment_regex = Str.regexp "^#.*fileinfo \\([^ ]+\\) \\(.*\\)$" in
   let rem_regex = Str.regexp "\\([^ ]+\\) [^ ]+ [^ ]+ \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)$" in
   let tm = Unix.localtime timestamp in
   let rem_date_str = (string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let remind_channel = Unix.open_process_in ("remind -s -l -g -b2 " ^ !Rcfile.reminders_file ^
   " " ^ rem_date_str) in
   let rec build_lists timed untimed =
      try
         let line = input_line remind_channel in
         if Str.string_match comment_regex line 0 then begin
            let line_num_s = Str.matched_group 1 line
            and filename   = Str.matched_group 2 line in
            let rem_line   = input_line remind_channel in
            if Str.string_match rem_regex rem_line 0 then begin
               let date_s     = Str.matched_group 1 rem_line
               and duration_s = Str.matched_group 2 rem_line
               and min_s      = Str.matched_group 3 rem_line
               and msg        = Str.matched_group 4 rem_line in
               (* further subdivide the date string *)
               let date_arr = Array.of_list (Str.split (Str.regexp "/") date_s) in
               let year     = int_of_string date_arr.(0)
               and month    = int_of_string date_arr.(1)
               and day      = int_of_string date_arr.(2) in
               let temp = {
                  Unix.tm_sec   = 0;
                  Unix.tm_min   = 0;
                  Unix.tm_hour  = 0;
                  Unix.tm_mday  = day;
                  Unix.tm_mon   = pred month;
                  Unix.tm_year  = year - 1900;
                  Unix.tm_wday  = 0;
                  Unix.tm_yday  = 0;
                  Unix.tm_isdst = false
               } in
               if min_s = "*" then
                  let (f_rem_ts, _) = Unix.mktime temp in
                  build_lists timed ((f_rem_ts, msg, filename, line_num_s) :: untimed)
               else
                  let temp_with_min = {temp with Unix.tm_min = int_of_string min_s} in
                  let (f_rem_ts, _) = Unix.mktime temp_with_min in
                  let duration =
                     if duration_s = "*" then 0.0
                     else float_of_string duration_s
                  in
                  build_lists ((f_rem_ts, f_rem_ts +. (duration *. 60.), msg, 
                                filename, line_num_s) :: timed) untimed
            end else
               (* if there was no rem_regex match, continue with next line *)
               build_lists timed untimed
         end else
            (* if there was no comment_regex match, continue with next line *)
            build_lists timed untimed
      with
      | End_of_file ->
           let _ = Unix.close_process_in remind_channel in
           (List.rev timed, List.rev untimed)
      | _ ->
           (* if there's an error in regexp matching or string coersion,
            * just drop that reminder and go to the next line *)
           build_lists timed untimed
   in
   build_lists [] []


(* generate a count of how many reminders fall on any given day of
 * the month *)
let count_reminders timed untimed =
   let rem_counts = Array.make 31 0 in
   let count_timed (start, _, _, _, _) =
      let tm = Unix.localtime start in
      let day = pred tm.Unix.tm_mday in
      rem_counts.(day) <- succ rem_counts.(day)
   in
   let count_untimed (start, _, _, _) =
      let tm = Unix.localtime start in
      let day = pred tm.Unix.tm_mday in
      rem_counts.(day) <- succ rem_counts.(day)
   in
   List.iter count_timed timed;
   List.iter count_untimed untimed;
   rem_counts


(* use cal(1) to create a calendar record for the desired timestamp *)
let make_cal timestamp =
   let tm = Unix.localtime timestamp in
   let month_s = string_of_int (succ tm.Unix.tm_mon)
   and year_s  = string_of_int (tm.Unix.tm_year + 1900) in
   let command = 
      (* support Euro calendar style *)
      if !Rcfile.week_starts_monday then
         "cal -m " ^ month_s ^ " " ^ year_s
      else
         "cal " ^ month_s ^ " " ^ year_s
   in
   let cal_channel = Unix.open_process_in command in
   let rec add_lines day_lines =
      try
         let line = input_line cal_channel in
         add_lines (line :: day_lines)
      with End_of_file ->
         let _ = Unix.close_process_in cal_channel in
         List.rev day_lines
   in 
   let t  = input_line cal_channel in
   let wd = input_line cal_channel in
   let d  = add_lines [] in {
      title    = t;
      weekdays = wd;
      days     = d
   }


(* comparison functions for sorting reminders chronologically *)
let cmp_timed rem_a rem_b =
   let (ts_a, _, _, _, _) = rem_a
   and (ts_b, _, _, _, _) = rem_b in
   compare ts_a ts_b

let cmp_untimed rem_a rem_b =
   let (ts_a, _, _, _) = rem_a
   and (ts_b, _, _, _) = rem_b in
   compare ts_a ts_b


(* same thing as List.append (or @), but tail-recursive *)
let safe_append a b = List.rev_append (List.rev a) b


(* initialize a new three-month reminder record *)
let create_three_month timestamp =
   let temp = {
      Unix.localtime timestamp with Unix.tm_sec  = 0;
                                    Unix.tm_min  = 0;
                                    Unix.tm_hour = 0;
                                    Unix.tm_mday = 1
   } in
   let (curr_ts, _) = Unix.mktime temp in
   let temp_prev = {
      temp with Unix.tm_mon = pred temp.Unix.tm_mon
   } in
   let temp_next = {
      temp with Unix.tm_mon = succ temp.Unix.tm_mon
   } in
   let (prev_ts, _) = Unix.mktime temp_prev
   and (next_ts, _) = Unix.mktime temp_next in
   let (pt, pu) = month_reminders prev_ts in
   let (ct, cu) = month_reminders curr_ts in
   let (nt, nu) = month_reminders next_ts in {
      curr_timestamp = curr_ts;
      prev_timed     = pt;
      curr_timed     = ct;
      next_timed     = nt;
      all_timed      = safe_append pt (safe_append ct nt);
      prev_untimed   = pu;
      curr_untimed   = cu;
      next_untimed   = nu;
      all_untimed    = safe_append cu (safe_append pu nu);
      curr_counts    = count_reminders ct cu;
      curr_cal       = make_cal curr_ts
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
   let (new_curr_timestamp, _) = Unix.mktime temp1 in
   let (next_timestamp, _)     = Unix.mktime temp2 in
   let (t, u) = month_reminders next_timestamp in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = reminders.curr_timed;
      curr_timed     = reminders.next_timed;
      next_timed     = t;
      all_timed      = safe_append reminders.curr_timed (safe_append reminders.next_timed t);
      prev_untimed   = reminders.curr_untimed;
      curr_untimed   = reminders.next_untimed;
      next_untimed   = u;
      all_untimed    = safe_append reminders.curr_untimed (safe_append reminders.next_untimed u);
      curr_counts    = count_reminders reminders.next_timed reminders.next_untimed;
      curr_cal       = make_cal new_curr_timestamp
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
   let (new_curr_timestamp, _) = Unix.mktime temp1 in
   let (prev_timestamp, _)     = Unix.mktime temp2 in
   let (t, u) = month_reminders prev_timestamp in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = t;
      curr_timed     = reminders.prev_timed;
      next_timed     = reminders.curr_timed;
      all_timed      = safe_append t (safe_append reminders.prev_timed reminders.curr_timed);
      prev_untimed   = u;
      curr_untimed   = reminders.prev_untimed;
      next_untimed   = reminders.curr_untimed;
      all_untimed    = safe_append u (safe_append reminders.prev_untimed reminders.curr_untimed);
      curr_counts    = count_reminders reminders.prev_timed reminders.prev_untimed;
      curr_cal       = make_cal new_curr_timestamp
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
 * of that reminder. *)
let find_next msg_regex timestamp =
   let rem_regex_timed = 
      Str.regexp "^\\([^ ]+\\)/\\([^ ]+\\)/\\([^ ]+\\) \\([0-9]+\\):\\([0-9]+\\) \\([^ ]+.*\\)$"
   in
   let rem_regex_untimed = Str.regexp "^\\([^ ]+\\)/\\([^ ]+\\)/\\([^ ]+\\) \\([^ ]+.*\\)$" in
   let tm1 = Unix.localtime timestamp in
   let tm2 = Unix.localtime (timestamp +. 86400.) in      (* 24 hours *)
   let rem_date_str1 = (string_of_tm_mon tm1.Unix.tm_mon) ^ " " ^ 
                       (string_of_int tm1.Unix.tm_mday) ^ " " ^
                       (string_of_int (tm1.Unix.tm_year + 1900)) in
   let rem_date_str2 = (string_of_tm_mon tm2.Unix.tm_mon) ^ " " ^ 
                       (string_of_int tm2.Unix.tm_mday) ^ " " ^
                       (string_of_int (tm2.Unix.tm_year + 1900)) in
   let remind_channel = Unix.open_process_in ("remind -n -b1 " ^ !Rcfile.reminders_file ^
   " " ^ rem_date_str1 ^ " > /tmp/wyrd-tmp && remind -n -b1 " ^ !Rcfile.reminders_file ^
   " " ^ rem_date_str2 ^ " | cat /tmp/wyrd-tmp - | sort") in
   let rec check_messages () =
      try
         let line = input_line remind_channel in
         if Str.string_match rem_regex_timed line 0 then begin
            (* go here if this line is a timed reminder *)
            let year  = int_of_string (Str.matched_group 1 line)
            and month = int_of_string (Str.matched_group 2 line)
            and day   = int_of_string (Str.matched_group 3 line)
            and hour  = int_of_string (Str.matched_group 4 line)
            and min   = int_of_string (Str.matched_group 5 line)
            and msg   = Str.matched_group 6 line in
            let temp = {
               Unix.tm_sec   = 0;
               Unix.tm_min   = min;
               Unix.tm_hour  = hour;
               Unix.tm_mday  = day;
               Unix.tm_mon   = pred month;
               Unix.tm_year  = year - 1900;
               Unix.tm_wday  = 0;
               Unix.tm_yday  = 0;
               Unix.tm_isdst = false
            } in
            let (ts, _) = Unix.mktime temp in
            if ts > timestamp then
               begin try
                  let _ = Str.search_forward msg_regex msg 0 in
                  let _ = Unix.close_process_in remind_channel in
                  ts
               with Not_found ->
                  check_messages ()
               end
            else
               check_messages ()
         end else if Str.string_match rem_regex_untimed line 0 then begin
            (* go here if this line is an untimed reminder *)
            let year  = int_of_string (Str.matched_group 1 line)
            and month = int_of_string (Str.matched_group 2 line)
            and day   = int_of_string (Str.matched_group 3 line)
            and msg   = Str.matched_group 4 line in
            let temp = {
               Unix.tm_sec   = 0;
               Unix.tm_min   = 0;
               Unix.tm_hour  = 0;
               Unix.tm_mday  = day;
               Unix.tm_mon   = pred month;
               Unix.tm_year  = year - 1900;
               Unix.tm_wday  = 0;
               Unix.tm_yday  = 0;
               Unix.tm_isdst = false
            } in
            let (ts, _) = Unix.mktime temp in
            if ts > timestamp then
               begin try
                  let _ = Str.search_forward msg_regex msg 0 in
                  let _ = Unix.close_process_in remind_channel in
                  ts
               with Not_found ->
                  check_messages ()
               end
            else
               check_messages ()
         end else
            (* if there was no regexp match, continue with next line *)
            check_messages ()
      with
      | End_of_file ->
         let _ = Unix.close_process_in remind_channel in
         raise Not_found
      | Failure s ->
        (* if there's an error in string coersion, just drop that reminder and go
         * to the next line *)
        check_messages ()
   in
   check_messages ()


   

(* arch-tag: DO_NOT_CHANGE_6bb48a1c-2b0c-4254-ba3a-ee9b48007169 *)
