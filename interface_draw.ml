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
         let temp2 = {
            timestamp with
              Unix.tm_sec = 0;
              Unix.tm_min = ~- (time_inc iface);
              Unix.tm_hour = 0
         } in
         let (_, before_midnight) = Unix.mktime temp2 in
         if timestamp.Unix.tm_min = before_midnight.Unix.tm_min && 
            timestamp.Unix.tm_hour = before_midnight.Unix.tm_hour then
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


(* Draw the timed schedule.  The algorithm iterates across all reminders in
 * a three month period, and draws them in one by one.  An array is used
 * to keep track of which lines have not yet been drawn, so that these can
 * be filled in later. *)
let draw_timed iface reminders =
   let round_down x =
      if x >= 0.0 then int_of_float x
      else pred (int_of_float x)
   in
   let is_drawn = Array.make iface.scr.tw_lines false in
   let blank = String.make iface.scr.tw_cols ' ' in
   let (f_top, _) = Unix.mktime iface.top_timestamp in
   wattron iface.scr.timed_win ((WA.color_pair 3) lor WA.bold);
   let process_reminder (start, finish, msg) =
      let rem_top_line =
         round_down ((start -. f_top) /. (float_of_int (60 * (time_inc iface))))
      in
      (* draw the top line of a reminder *)
      if rem_top_line >= 0 && rem_top_line < iface.scr.tw_lines then begin
         let ts = timestamp_of_line iface rem_top_line in
         let ts_str = Printf.sprintf "%.2d:%.2d " ts.Unix.tm_hour ts.Unix.tm_min in
         (* FIXME: add ellipses if truncation occurs *)
         let s = Str.string_before (ts_str ^ msg ^ blank) (iface.scr.tw_cols - 2) in
         if rem_top_line = iface.left_selection then
            wattron iface.scr.timed_win WA.reverse
         else
            wattroff iface.scr.timed_win WA.reverse;
         is_drawn.(rem_top_line) <- true;
         assert (mvwaddstr iface.scr.timed_win rem_top_line 2 s)
      end else
         ();
      (* draw any remaining lines of this reminder, as determined by the duration *)
      let count = ref 1 in
      while 
         let (f_ts, _) = 
            Unix.mktime (timestamp_of_line iface (rem_top_line + !count))
         in
         (f_ts < finish) && (rem_top_line + !count < iface.scr.tw_lines)
      do
         if rem_top_line + !count >= 0 then begin
            let (_, ts) = 
               Unix.mktime (timestamp_of_line iface (rem_top_line + !count))
            in
            let ts_str = Printf.sprintf "%.2d:%.2d " ts.Unix.tm_hour ts.Unix.tm_min in
            let s = Str.string_before (ts_str ^ blank) (iface.scr.tw_cols - 2) in
            if rem_top_line + !count = iface.left_selection then
               wattron iface.scr.timed_win WA.reverse
            else
               wattroff iface.scr.timed_win WA.reverse;
            if not is_drawn.(rem_top_line + !count) then begin
               is_drawn.(rem_top_line + !count) <- true;
               assert (mvwaddstr iface.scr.timed_win (rem_top_line + !count) 2 s)
            end else
               ();
         end else
            ();
         count := succ !count
      done
   in
   List.iter process_reminder reminders;
   wattroff iface.scr.timed_win ((WA.color_pair 3) lor WA.bold lor WA.reverse);
   (* finish off by drawing in the blank timeslots *)
   for i = 0 to pred iface.scr.tw_lines do
      if not is_drawn.(i) then begin
         if i = iface.left_selection then
            wattron iface.scr.timed_win WA.reverse
         else
            wattroff iface.scr.timed_win WA.reverse;
         let ts = timestamp_of_line iface i in
         let ts_str = Printf.sprintf "%.2d:%.2d " ts.Unix.tm_hour ts.Unix.tm_min in
         let s = Str.string_before (ts_str ^ blank) (iface.scr.tw_cols - 2) in
         assert (mvwaddstr iface.scr.timed_win i 2 s)
      end else
         ()
   done;
   wattroff iface.scr.timed_win WA.reverse;
   assert (wnoutrefresh iface.scr.timed_win)





(* arch-tag: DO_NOT_CHANGE_9ff0fd0c-6eb1-410f-8fcf-6dfcf94b346a *)
