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
            let year     = int_of_string date_arr.(0) in
            let month    = int_of_string date_arr.(1) in
            let day      = int_of_string date_arr.(2) in
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
            if duration_s = "*" then
               let (_, reminder_timestamp) = Unix.mktime temp in
               build_lists timed ((reminder_timestamp, msg) :: untimed)
            else
               let temp_with_min = {temp with Unix.tm_min = int_of_string min_s} in
               let (_, reminder_timestamp) = Unix.mktime temp_with_min in
               let duration = int_of_string duration_s in
               build_lists ((reminder_timestamp, duration, msg) :: timed) untimed
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






(* arch-tag: DO_NOT_CHANGE_6bb48a1c-2b0c-4254-ba3a-ee9b48007169 *)
