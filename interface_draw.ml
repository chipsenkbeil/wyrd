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


(* interface_draw.ml
 * All drawing operations are found here.
 *)

open Interface
open Curses
open Remind




(* Draw the one-line help window at the top of the screen *)
let draw_help (iface : interface_state_t) =
   wattron iface.scr.help_win ((WA.color_pair 1) lor WA.bold lor WA.underline);
   let s = "t:new timed    u:new untimed    <enter>:edit    z:zoom" in
   let blanks = String.make (iface.scr.hw_cols - (String.length s)) ' ' in
   assert (mvwaddstr iface.scr.help_win 0 0 (s ^ blanks));
   assert (wnoutrefresh iface.scr.help_win)


let time_inc iface =
   match iface.zoom_level with
   | Hour        -> 60
   | HalfHour    -> 30
   | QuarterHour -> 15


(* Draw the vertical date strip at the left of the timed window.
 * Note: non-trivial.
 * The first date stamp is a special case; it is drawn at the top
 * of the screen and truncated at the beginning to fit before the
 * first date change.  The remaining date stamps are all drawn immediately
 * after any date changes, and truncated at the end to fit in the
 * window.
 * Step 1: determine the line numbers on which the dates change
 * Step 2: create a string to represent the vertical strip
 * Step 3: draw each of the characters of the string onto the window *)
let draw_date_strip (iface : interface_state_t) =
   (* draw the vertical line to the right of the date string *)
   let acs = get_acs_codes () in
   wattron iface.scr.timed_win ((WA.color_pair 5) lor WA.bold);
   mvwvline iface.scr.timed_win 0 1 acs.Acs.vline iface.scr.tw_lines;
   wattroff iface.scr.timed_win (WA.color_pair 5);
   (* determine the line numbers and timestamps of any date changes within the
    * timed window *)
   let rec check_timestamp date_changes timestamp line =
      if line >= iface.scr.tw_lines then
         date_changes
      else
         let temp = {
            timestamp with 
              Unix.tm_min = timestamp.Unix.tm_min + (time_inc iface)
         } in
         let (_, next_timestamp) = Unix.mktime temp in
         if timestamp.Unix.tm_min = 0 && timestamp.Unix.tm_hour = 0 then
            check_timestamp ((line, timestamp) :: date_changes) next_timestamp (succ line)
         else
            check_timestamp date_changes next_timestamp (succ line)
   in
   let date_changes = List.rev (check_timestamp [] iface.top_timestamp 0) in
   (* generate a string to represent the vertical strip *)
   let date_chars = 
      if List.length date_changes > 0 then begin
         (* special case for the top date string, which is always at the
          * top of the screen *)
         let (line, timestamp) = List.hd date_changes in
         let top_date_str = 
            let temp = {
               timestamp with Unix.tm_mday = pred timestamp.Unix.tm_mday
            } in
            let (_, prev_day) = Unix.mktime temp in
            if line >= 7 then
               (* the date will fit completely *)
               (Printf.sprintf " %s %.2d" (string_of_tm_mon prev_day.Unix.tm_mon) 
                   prev_day.Unix.tm_mday) ^ (String.make (line - 7) ' ')
            else
               (* there's not enough room for the date, so truncate it *)
               Str.last_chars
               (Printf.sprintf " %s %.2d" (string_of_tm_mon prev_day.Unix.tm_mon) 
                   prev_day.Unix.tm_mday) line
         in
         (* all other dates are just rendered at the top of their respective windows *)
         let rec add_date date_str changes =
            match changes with
            | [] -> date_str
            | (line, timestamp) :: tail -> 
               let s_len = 
                  if List.length tail > 0 then
                     let (next_line, _) = List.hd tail in
                     next_line - line
                  else
                     iface.scr.tw_lines - line
               in
               let temp_s = 
                  (Printf.sprintf "-%s %.2d" (string_of_tm_mon timestamp.Unix.tm_mon) 
                      timestamp.Unix.tm_mday) ^ (String.make 100 ' ')
               in
               add_date (date_str ^ (Str.string_before temp_s s_len)) tail
         in
         add_date top_date_str date_changes
      end else
         (* if there are no date changes (e.g. for small window) then just grab the proper
          * date from the top_timestamp *)
         (Printf.sprintf "%s %.2d" (string_of_tm_mon iface.top_timestamp.Unix.tm_mon) 
             iface.top_timestamp.Unix.tm_mday) ^ (String.make (iface.scr.tw_lines - 6) ' ') 
   in
   (* draw the date string vertically, one character at a time *)
   for i = 0 to pred iface.scr.tw_lines do
      if date_chars.[i] = '-' then begin
         wattron iface.scr.timed_win (WA.color_pair 5);
         assert (mvwaddch iface.scr.timed_win i 0 acs.Acs.hline);
         assert (mvwaddch iface.scr.timed_win i 1 acs.Acs.rtee);
         wattroff iface.scr.timed_win (WA.color_pair 5);
      end else begin
         wattron iface.scr.timed_win (WA.color_pair 4);
         assert (mvwaddch iface.scr.timed_win i 0 (int_of_char date_chars.[i]));
         wattroff iface.scr.timed_win (WA.color_pair 4)
      end
   done;
   wattroff iface.scr.timed_win WA.bold;
   assert (wnoutrefresh iface.scr.timed_win)



let draw_timed iface reminders =
   let blank = String.make iface.scr.tw_cols ' ' in
   for i = 0 to pred iface.scr.tw_lines do
      if iface.selected_side = Left && i = iface.left_selection then
         wattron iface.scr.timed_win WA.reverse
      else
         wattroff iface.scr.timed_win WA.bold;
      let temp1 = {
         iface.top_timestamp with
           Unix.tm_min = iface.top_timestamp.Unix.tm_min + (i * (time_inc iface))
      } in
      let temp2 = {
         iface.top_timestamp with
           Unix.tm_min = iface.top_timestamp.Unix.tm_min + ((succ i) * (time_inc iface))
      } in
      let (f_curr_ts, curr_ts) = Unix.mktime temp1
      and (f_next_ts, next_ts) = Unix.mktime temp2 in
      let ts_str = Printf.sprintf "%.2d:%.2d " curr_ts.Unix.tm_hour curr_ts.Unix.tm_min in
      let blank_ts = Str.string_before (ts_str ^ blank) (iface.scr.tw_cols - 2) in
      (* iterate through all of this month's reminders to determine whether anything
       * special has to be drawn on this line *)
      let has_msg     = ref false in
      let has_overlap = ref false in
      let test_reminder (start, finish, rem_msg) =
         (* test for start of a timed reminder *)
         if start >= f_curr_ts && start < f_next_ts then begin
            wattron iface.scr.timed_win ((WA.color_pair 3) lor WA.bold);
            let s = ts_str ^ rem_msg in
            let trunc_s = 
               if String.length s > iface.scr.tw_cols - 3 then
                  (Str.string_before s (iface.scr.tw_cols - 5)) ^ "..."
               else
                  Str.string_before (s ^ blank) (iface.scr.tw_cols - 2)
            in
            has_msg := true;
            assert (mvwaddstr iface.scr.timed_win i 2 trunc_s)
         (* test for a timed reminder that started earlier, but has duration
          * carrying it over this timeslot *)
         end else if (not !has_msg) && start < f_curr_ts && finish > f_curr_ts then begin
            wattron iface.scr.timed_win ((WA.color_pair 3) lor WA.bold);
            has_overlap := true;
            assert (mvwaddstr iface.scr.timed_win i 2 blank_ts)
         end else
            ()
      in
      List.iter test_reminder reminders;
      if (not !has_msg) && (not !has_overlap) then begin
         wattron iface.scr.timed_win (WA.color_pair 0);
         assert (mvwaddstr iface.scr.timed_win i 2 blank_ts)
      end else
         ();
      wattroff iface.scr.timed_win ((WA.color_pair 3) lor WA.bold lor WA.reverse)
   done;
   assert (wnoutrefresh iface.scr.timed_win)






      
      
      





(* arch-tag: DO_NOT_CHANGE_9ff0fd0c-6eb1-410f-8fcf-6dfcf94b346a *)
