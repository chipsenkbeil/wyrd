(*  Remic -- a curses-based front-end for Remind
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

(* Storage for a three-month window of reminders.
 * Makes it possible to handle edge effects of moving
 * from month to month without constantly calling 'rem'. *)
type three_month_rem_t = {
   curr_timestamp : Unix.tm;
   prev_timed     : (float * float * string) list;
   curr_timed     : (float * float * string) list;
   next_timed     : (float * float * string) list;
   all_timed      : (float * float * string) list;
   prev_untimed   : (float * string) list;
   curr_untimed   : (float * string) list;
   next_untimed   : (float * string) list;
   all_untimed    : (float * string) list
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




(* Obtain two lists of reminders for the month of the timestamp argument.
 * The first list is for timed reminders, the second is for untimed. *)
let month_reminders timestamp =
   let rem_regex = Str.regexp "^\\([^ ]+\\) [^ ]+ [^ ]+ \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)$" in
   let rem_date_str = (string_of_tm_mon timestamp.Unix.tm_mon) ^ " " ^ 
                      (string_of_int timestamp.Unix.tm_mday) ^ " " ^
                      (string_of_int (timestamp.Unix.tm_year + 1900)) in
   let remind_channel = Unix.open_process_in ("rem -s -b2 " ^ rem_date_str) in
   let rec build_lists timed untimed =
      try
         let line = input_line remind_channel in
         if Str.string_match rem_regex line 0 then begin
            let date_s     = Str.matched_group 1 line
            and duration_s = Str.matched_group 2 line
            and min_s      = Str.matched_group 3 line
            and msg        = Str.matched_group 4 line in
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
               build_lists timed ((f_rem_ts, msg) :: untimed)
            else
               let temp_with_min = {temp with Unix.tm_min = int_of_string min_s} in
               let (f_rem_ts, _) = Unix.mktime temp_with_min in
               let duration =
                  if duration_s = "*" then 0.0
                  else float_of_string duration_s
               in
               build_lists ((f_rem_ts, f_rem_ts +. (duration *. 60.), msg) :: timed) untimed
         end else
            (* if there was no regexp match, continue with next line *)
            build_lists timed untimed
      with
      | End_of_file ->
           (timed, untimed)
      | _ ->
           (* if there's an error in regexp matching or string coersion,
            * just drop that reminder and go to the next line *)
           build_lists timed untimed
   in
   build_lists [] []


(* initialize a new three-month reminder record *)
let create_three_month timestamp =
   let temp = {
      timestamp with Unix.tm_sec = 0;
                     Unix.tm_min = 0;
                     Unix.tm_hour = 0;
                     Unix.tm_mday = 1
   } in
   let (_, curr_ts) = Unix.mktime temp in
   let temp_prev = {
      temp with Unix.tm_mon = pred temp.Unix.tm_mon
   } in
   let temp_next = {
      temp with Unix.tm_mon = succ temp.Unix.tm_mon
   } in
   let (_, prev_ts) = Unix.mktime temp_prev
   and (_, next_ts) = Unix.mktime temp_next in
   let (pt, pu) = month_reminders prev_ts in
   let (ct, cu) = month_reminders curr_ts in
   let (nt, nu) = month_reminders next_ts in {
      curr_timestamp = curr_ts;
      prev_timed     = pt;
      curr_timed     = ct;
      next_timed     = nt;
      all_timed      = List.rev_append (List.rev_append ct pt) nt;
      prev_untimed   = pu;
      curr_untimed   = cu;
      next_untimed   = nu;
      all_untimed    = List.rev_append (List.rev_append cu pu) nu
   }



(* Update a three-month reminders record for the next month *)
let next_month reminders =
   let temp1 = {
      reminders.curr_timestamp with 
        Unix.tm_mon = succ reminders.curr_timestamp.Unix.tm_mon
   } in
   let temp2 = {
      reminders.curr_timestamp with 
        Unix.tm_mon = reminders.curr_timestamp.Unix.tm_mon + 2
   } in
   let (_, new_curr_timestamp) = Unix.mktime temp1 in
   let (_, next_timestamp) = Unix.mktime temp2 in
   let (t, u) = month_reminders next_timestamp in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = reminders.curr_timed;
      curr_timed     = reminders.next_timed;
      next_timed     = t;
      all_timed      = List.rev_append 
                          (List.rev_append reminders.next_timed reminders.curr_timed) t;
      prev_untimed   = reminders.curr_untimed;
      curr_untimed   = reminders.next_untimed;
      next_untimed   = u;
      all_untimed    = List.rev_append 
                          (List.rev_append reminders.next_untimed reminders.curr_untimed) u
   }


(* Update a three-month reminders record for the previous month *)
let prev_month reminders =
   let temp1 = {
      reminders.curr_timestamp with 
        Unix.tm_mon = pred reminders.curr_timestamp.Unix.tm_mon
   } in
   let temp2 = {
      reminders.curr_timestamp with 
        Unix.tm_mon = reminders.curr_timestamp.Unix.tm_mon - 2
   } in
   let (_, new_curr_timestamp) = Unix.mktime temp1 in
   let (_, prev_timestamp) = Unix.mktime temp2 in
   let (t, u) = month_reminders prev_timestamp in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = t;
      curr_timed     = reminders.prev_timed;
      next_timed     = reminders.curr_timed;
      all_timed      = List.rev_append 
                          (List.rev_append reminders.prev_timed t) reminders.curr_timed;
      prev_untimed   = u;
      curr_untimed   = reminders.prev_untimed;
      next_untimed   = reminders.curr_untimed;
      all_untimed    = List.rev_append 
                          (List.rev_append reminders.prev_untimed u) reminders.curr_untimed
   }


(* Return a new reminders record centered on the current timestamp,
 * doing as little work as possible. *)
let update_reminders rem timestamp =
   if timestamp.Unix.tm_year = rem.curr_timestamp.Unix.tm_year &&
      timestamp.Unix.tm_mon  = rem.curr_timestamp.Unix.tm_mon &&
      timestamp.Unix.tm_mday = rem.curr_timestamp.Unix.tm_mday then
      rem
   else
      let temp1 = {
         rem.curr_timestamp with Unix.tm_mon = pred rem.curr_timestamp.Unix.tm_mon
      } in
      let temp2 = {
         rem.curr_timestamp with Unix.tm_mon = succ rem.curr_timestamp.Unix.tm_mon
      } in
      let (_, prev_timestamp) = Unix.mktime temp1 in
      let (_, next_timestamp) = Unix.mktime temp2 in
      if timestamp.Unix.tm_year = prev_timestamp.Unix.tm_year &&
         timestamp.Unix.tm_mon  = prev_timestamp.Unix.tm_mon &&
         timestamp.Unix.tm_mday = prev_timestamp.Unix.tm_mday then
         prev_month rem
      else if timestamp.Unix.tm_year = next_timestamp.Unix.tm_year &&
         timestamp.Unix.tm_mon  = next_timestamp.Unix.tm_mon &&
         timestamp.Unix.tm_mday = next_timestamp.Unix.tm_mday then
         next_month rem
      else
         create_three_month timestamp





(* arch-tag: DO_NOT_CHANGE_6bb48a1c-2b0c-4254-ba3a-ee9b48007169 *)
