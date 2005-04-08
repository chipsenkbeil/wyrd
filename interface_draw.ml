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


(* interface_draw.ml
 * All drawing operations are found here.
 *)

open Interface
open Curses
open Remind



(* Word-wrap a string--split the string at whitespace boundaries
 * to form a list of strings, each of which has length less than
 * 'len'. *)
let word_wrap (s : string) (len : int) =
   let ws = Str.regexp "[\t ]+" in
   let split_words = Str.split ws s in
   let rec process_words words lines =
      match words with
      |[] ->
         List.rev lines
      |word :: remaining_words ->
         let word_len = String.length word in
         begin match lines with
         |[] ->
            process_words words [""]
         |line :: remaining_lines ->
            let line_len = String.length line in
            if word_len + line_len + 1 <= len then
               if line_len = 0 then
                  process_words remaining_words (word :: remaining_lines)
               else
                  process_words remaining_words ((line ^ " " ^ word) :: remaining_lines)
            else if word_len <= len then
               process_words remaining_words (word :: line :: remaining_lines)
            else
               let front = Str.string_before word len
               and back  = Str.string_after  word len in
               process_words (back :: remaining_words) 
                  (front :: line :: remaining_lines)
         end
   in
   process_words split_words []


(* Generate a 12-hour clock representation of a time record *)
let twelve_hour_string tm =
   if tm.Unix.tm_hour >= 12 then 
      let hour = tm.Unix.tm_hour - 12 in
      if hour = 0 then
         Printf.sprintf "12:%.2dpm" tm.Unix.tm_min
      else
         Printf.sprintf "%d:%.2dpm" hour tm.Unix.tm_min
   else
      if tm.Unix.tm_hour = 0 then
         Printf.sprintf "12:%.2dam" tm.Unix.tm_min
      else
         Printf.sprintf "%d:%.2dam" tm.Unix.tm_hour tm.Unix.tm_min


(* Draw a string in a specified window and location, using exactly 'len'
 * characters and truncating with ellipses if necessary. *)
let trunc_mvwaddstr win line col len s =
   let fixed_s =
      if String.length s <= len then
         let pad = String.make (len - (String.length s)) ' ' in
         s ^ pad
      else
         (Str.string_before s (len - 3)) ^ "..."
   in
   assert (mvwaddstr win line col fixed_s)


(* Draw the one-line help window at the top of the screen *)
let draw_help (iface : interface_state_t) =
   wattron iface.scr.help_win ((WA.color_pair 1) lor WA.bold lor WA.underline);
   let rec build_help_line operations s =
      match operations with
      |[] -> 
         s
      |(op, op_string) :: tail ->
         try
            let key_string = Rcfile.key_of_command op in
            build_help_line tail (s ^ key_string ^ ":" ^ op_string ^ "   ")
         with Not_found ->
            build_help_line tail s
   in
   let help_string = 
      build_help_line [(Rcfile.NewTimed, "new timed"); (Rcfile.NewUntimed, "new untimed");
                       (Rcfile.Edit, "edit"); (Rcfile.Home, "home"); (Rcfile.Zoom, "zoom"); 
                       (Rcfile.BeginSearch, "search"); (Rcfile.Quit, "quit")] ""
   in
   trunc_mvwaddstr iface.scr.help_win 0 0 iface.scr.hw_cols help_string;
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
         let timestamp_tm = Unix.localtime timestamp in
         let next_timestamp = timestamp +. (time_inc iface) in
         let temp = {
            timestamp_tm with
              Unix.tm_sec = 0;
              Unix.tm_min = ~- (time_inc_min iface);
              Unix.tm_hour = 0
         } in
         let (_, before_midnight) = Unix.mktime temp in
         if timestamp_tm.Unix.tm_min = before_midnight.Unix.tm_min && 
            timestamp_tm.Unix.tm_hour = before_midnight.Unix.tm_hour then
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
         let tm = Unix.localtime timestamp in
         let top_date_str = 
            if line >= 7 then
               (* the date will fit completely *)
               (Printf.sprintf " %s %.2d" (string_of_tm_mon tm.Unix.tm_mon) 
                   tm.Unix.tm_mday) ^ (String.make (line - 7) ' ')
            else
               (* there's not enough room for the date, so truncate it *)
               Str.last_chars
               (Printf.sprintf " %s %.2d" (string_of_tm_mon tm.Unix.tm_mon) 
                   tm.Unix.tm_mday) line
         in
         (* all other dates are just rendered at the top of their respective windows *)
         let rec add_date date_str changes =
            match changes with
            | [] -> date_str
            | (line, timestamp) :: tail -> 
               let tm = Unix.localtime timestamp in
               let temp = {
                  tm with Unix.tm_mday = succ tm.Unix.tm_mday
               } in
               let (_, next_day) = Unix.mktime temp in
               let s_len = 
                  if List.length tail > 0 then
                     let (next_line, _) = List.hd tail in
                     next_line - line
                  else
                     iface.scr.tw_lines - line
               in
               let temp_s = 
                  (Printf.sprintf "-%s %.2d" (string_of_tm_mon next_day.Unix.tm_mon) 
                      next_day.Unix.tm_mday) ^ (String.make 100 ' ')
               in
               add_date (date_str ^ (Str.string_before temp_s s_len)) tail
         in
         add_date top_date_str date_changes
      end else
         (* if there are no date changes (e.g. for small window) then just grab the proper
          * date from the top_timestamp *)
         let tm = Unix.localtime iface.top_timestamp in
         (Printf.sprintf " %s %.2d" (string_of_tm_mon tm.Unix.tm_mon) 
             tm.Unix.tm_mday) ^ (String.make (iface.scr.tw_lines - 6) ' ') 
   in
   (* draw the date string vertically, one character at a time *)
   for i = 0 to pred iface.scr.tw_lines do
      if date_chars.[i] = '-' then begin
         wattron iface.scr.timed_win ((WA.color_pair 4) lor WA.underline);
         assert (mvwaddch iface.scr.timed_win i 0 acs.Acs.hline);
         wattroff iface.scr.timed_win ((WA.color_pair 4) lor WA.underline);
         wattron iface.scr.timed_win ((WA.color_pair 5) lor WA.underline);
         assert (mvwaddch iface.scr.timed_win i 1 acs.Acs.rtee);
         wattroff iface.scr.timed_win ((WA.color_pair 5) lor WA.underline)
      end else begin
         wattron iface.scr.timed_win (WA.color_pair 4);
         assert (mvwaddch iface.scr.timed_win i 0 (int_of_char date_chars.[i]));
         wattroff iface.scr.timed_win (WA.color_pair 4)
      end
   done;
   wattroff iface.scr.timed_win (WA.bold lor WA.underline);
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
   let lineinfo = Array.make iface.scr.tw_lines None in
   let blank = String.make iface.scr.tw_cols ' ' in
   wattron iface.scr.timed_win ((WA.color_pair 3) lor WA.bold);
   let top_tm = Unix.localtime iface.top_timestamp in
   let temp = {
      top_tm with
        Unix.tm_sec = 0;
        Unix.tm_min = ~- (time_inc_min iface);
        Unix.tm_hour = 0
   } in
   let (_, before_midnight) = Unix.mktime temp in
   let process_reminder (start, finish, msg, filename, line_num) =
      let rem_top_line =
         round_down ((start -. iface.top_timestamp) /. (time_inc iface))
      in
      let time_str =
         if finish > start then
            (twelve_hour_string (Unix.localtime start)) ^ "-" ^
            (twelve_hour_string (Unix.localtime finish)) ^ " "
         else
            (twelve_hour_string (Unix.localtime start)) ^ " "
      in
      (* draw the top line of a reminder *)
      if rem_top_line >= 0 && rem_top_line < iface.scr.tw_lines then begin
         let ts = timestamp_of_line iface rem_top_line in
         let tm = Unix.localtime ts in
         let ts_str = Printf.sprintf "%.2d:%.2d " tm.Unix.tm_hour tm.Unix.tm_min in
         if rem_top_line = iface.left_selection && iface.selected_side = Left then
            wattron iface.scr.timed_win WA.reverse
         else
            wattroff iface.scr.timed_win WA.reverse;
         if tm.Unix.tm_hour = before_midnight.Unix.tm_hour &&
            tm.Unix.tm_min  = before_midnight.Unix.tm_min then
            wattron iface.scr.timed_win WA.underline
         else
            wattroff iface.scr.timed_win WA.underline;
         is_drawn.(rem_top_line) <- true;
         lineinfo.(rem_top_line) <- Some (filename, line_num, time_str ^ msg);
         trunc_mvwaddstr iface.scr.timed_win rem_top_line 2 (iface.scr.tw_cols - 2) 
            (ts_str ^ msg)
      end else
         ();
      (* draw any remaining lines of this reminder, as determined by the duration *)
      let count = ref 1 in
      while 
         ((timestamp_of_line iface (rem_top_line + !count)) < finish) &&
         (rem_top_line + !count < iface.scr.tw_lines)
      do
         if rem_top_line + !count >= 0 then begin
            let ts = timestamp_of_line iface (rem_top_line + !count) in
            let tm = Unix.localtime ts in
            let ts_str = Printf.sprintf "%.2d:%.2d " tm.Unix.tm_hour tm.Unix.tm_min in
            let s = Str.string_before (ts_str ^ blank) (iface.scr.tw_cols - 2) in
            if rem_top_line + !count = iface.left_selection && 
               iface.selected_side = Left then
               wattron iface.scr.timed_win WA.reverse
            else
               wattroff iface.scr.timed_win WA.reverse;
            if tm.Unix.tm_hour = before_midnight.Unix.tm_hour &&
               tm.Unix.tm_min  = before_midnight.Unix.tm_min then
               wattron iface.scr.timed_win WA.underline
            else
               wattroff iface.scr.timed_win WA.underline;
            if not is_drawn.(rem_top_line + !count) then begin
               is_drawn.(rem_top_line + !count)  <- true;
               lineinfo.(rem_top_line + !count) <- 
                  Some (filename, line_num, time_str ^ msg);
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
         if i = iface.left_selection && iface.selected_side = Left then
            wattron iface.scr.timed_win WA.reverse
         else
            wattroff iface.scr.timed_win WA.reverse;
         let ts = timestamp_of_line iface i in
         let tm = Unix.localtime ts in
         let ts_str = Printf.sprintf "%.2d:%.2d " tm.Unix.tm_hour tm.Unix.tm_min in
         let s = Str.string_before (ts_str ^ blank) (iface.scr.tw_cols - 2) in
         if tm.Unix.tm_hour = before_midnight.Unix.tm_hour &&
            tm.Unix.tm_min  = before_midnight.Unix.tm_min then
            wattron iface.scr.timed_win WA.underline
         else
            wattroff iface.scr.timed_win WA.underline;
         assert (mvwaddstr iface.scr.timed_win i 2 s)
      end else
         ()
   done;
   wattroff iface.scr.timed_win (WA.reverse lor WA.underline);
   assert (wnoutrefresh iface.scr.timed_win);
   {iface with timed_lineinfo = lineinfo}



(* render a calendar for the given reminders record *)
let draw_calendar (iface : interface_state_t) 
       (reminders : three_month_rem_t) : unit =
   let curr_tm = Unix.localtime (timestamp_of_line iface iface.left_selection) in
   let cal = reminders.curr_cal in
   let acs = get_acs_codes () in
   wattron iface.scr.calendar_win ((WA.color_pair 5) lor WA.bold);
   mvwvline iface.scr.calendar_win 0 0 acs.Acs.vline iface.scr.cw_lines;
   wattroff iface.scr.calendar_win (WA.color_pair 5);
   let hspacer = (iface.scr.cw_cols - 20) / 2 in
   let vspacer = (iface.scr.cw_lines - 8) / 2 in
   assert (wmove iface.scr.calendar_win vspacer hspacer);
   wclrtoeol iface.scr.calendar_win;
   wattron iface.scr.calendar_win WA.bold;
   assert (waddstr iface.scr.calendar_win cal.title);
   wattroff iface.scr.calendar_win WA.bold;
   assert (wmove iface.scr.calendar_win (vspacer + 1) hspacer);
   wclrtoeol iface.scr.calendar_win;
   assert (waddstr iface.scr.calendar_win cal.weekdays);
   (* draw the day numbers *)
   let ws = Str.regexp " " in
   let rec draw_week weeks line =
      match weeks with
      | [] ->
         assert (wmove iface.scr.calendar_win line hspacer);
         wclrtoeol iface.scr.calendar_win
      | week :: tail ->
         let split_week = Str.full_split ws week in
         assert (wmove iface.scr.calendar_win line hspacer);
         wclrtoeol iface.scr.calendar_win;
         let rec draw_el elements =
            match elements with
            | [] ->
               ()
            | el :: days ->
               begin match el with
               |Str.Delim s -> 
                  assert (waddstr iface.scr.calendar_win s)
               |Str.Text d ->
                  let day = pred (int_of_string d) in
                  if succ day = curr_tm.Unix.tm_mday then begin
                     (* highlight selected day *)
                     wattron iface.scr.calendar_win ((WA.color_pair 2) lor WA.reverse);
                     assert (waddstr iface.scr.calendar_win d);
                     wattroff iface.scr.calendar_win ((WA.color_pair 2) lor WA.reverse)
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level1 then
                     assert (waddstr iface.scr.calendar_win d)
                  else if reminders.curr_counts.(day) <= !Rcfile.busy_level2  then begin
                     wattron iface.scr.calendar_win (WA.color_pair 7);
                     assert (waddstr iface.scr.calendar_win d);
                     wattroff iface.scr.calendar_win (WA.color_pair 7)
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level3 then begin
                     wattron iface.scr.calendar_win ((WA.color_pair 7) lor WA.bold);
                     assert (waddstr iface.scr.calendar_win d);
                     wattroff iface.scr.calendar_win ((WA.color_pair 7) lor WA.bold)
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level4 then begin
                     wattron iface.scr.calendar_win (WA.color_pair 9);
                     assert (waddstr iface.scr.calendar_win d);
                     wattroff iface.scr.calendar_win (WA.color_pair 9)
                  end else begin
                     wattron iface.scr.calendar_win ((WA.color_pair 9) lor WA.bold);
                     assert (waddstr iface.scr.calendar_win d);
                     wattroff iface.scr.calendar_win ((WA.color_pair 9) lor WA.bold)
                  end
               end;
               draw_el days
         in
         draw_el split_week;
         draw_week tail (succ line)
   in
   draw_week cal.days (vspacer + 2);
   assert (wnoutrefresh iface.scr.calendar_win)



(* Draw the untimed reminders window *)
let draw_untimed (iface : interface_state_t) reminders =
   let lineinfo = Array.make iface.scr.uw_lines None in
   let blank = String.make iface.scr.uw_cols ' ' in
   let curr_ts = Unix.localtime (timestamp_of_line iface iface.left_selection) in
   let temp1 = {
      curr_ts with Unix.tm_sec  = 0;
                   Unix.tm_min  = 0;
                   Unix.tm_hour = 0
   } in
   let temp2 = {
      curr_ts with Unix.tm_sec  = 0;
                   Unix.tm_min  = 0;
                   Unix.tm_hour = 0;
                   Unix.tm_mday = succ curr_ts.Unix.tm_mday 
   } in
   let (day_start_ts, _) = Unix.mktime temp1 in
   let (day_end_ts, _)   = Unix.mktime temp2 in
   werase iface.scr.untimed_win;
   let acs = get_acs_codes () in
   wattron iface.scr.untimed_win ((WA.color_pair 5) lor WA.bold);
   assert (mvwaddch iface.scr.untimed_win 0 0 acs.Acs.ltee);
   mvwhline iface.scr.untimed_win 0 1 acs.Acs.hline (pred iface.scr.uw_cols);
   mvwvline iface.scr.untimed_win 1 0 acs.Acs.vline (pred iface.scr.uw_lines);
   wattroff iface.scr.untimed_win (WA.color_pair 5);
   (* Filter the reminders list to find only those corresponding to the
    * selected day *)
   let is_current (rem_ts, msg, filename, line_num) =
      rem_ts >= day_start_ts && rem_ts < day_end_ts
   in
   let today_reminders = List.filter is_current reminders in
   let rec render_lines rem_list n line =
      match rem_list with
      |[] ->
         ()
      |(rem_ts, msg, filename, line_num) :: tail ->
         if line < iface.scr.uw_lines then
            if n >= iface.top_untimed then begin
               if line = iface.right_selection && iface.selected_side = Right then
                  wattron iface.scr.untimed_win WA.reverse
               else
                  wattroff iface.scr.untimed_win WA.reverse;
               trunc_mvwaddstr iface.scr.untimed_win line 2 (iface.scr.uw_cols - 3) 
                  ("* " ^ msg);
               lineinfo.(line) <- Some (filename, line_num, msg);
               render_lines tail (succ n) (succ line)
            end else
               render_lines tail (succ n) line
         else
            ()
   in
   render_lines today_reminders 0 1;
   wattroff iface.scr.untimed_win (WA.bold lor WA.reverse);
   (* if there's not enough window space to display all untimed reminders, display
    * arrows to indicate scrolling ability *)
   if iface.top_untimed > 0 then begin
      assert (mvwaddch iface.scr.untimed_win 1 (pred iface.scr.uw_cols) (int_of_char '^'));
      assert (mvwaddch iface.scr.untimed_win 2 (pred iface.scr.uw_cols) (int_of_char '^'))
   end else
      ();
   if List.length today_reminders > pred iface.scr.uw_lines + iface.top_untimed then begin
      assert (mvwaddch iface.scr.untimed_win (pred iface.scr.uw_lines) 
             (pred iface.scr.uw_cols) (int_of_char 'v'));
      assert (mvwaddch iface.scr.untimed_win (iface.scr.uw_lines - 2) 
             (pred iface.scr.uw_cols) (int_of_char 'v'))
   end else
      ();
   assert (wnoutrefresh iface.scr.untimed_win);
   {iface with untimed_lineinfo = lineinfo;
               len_untimed      = List.length today_reminders}


(* Draw the message window *)
let draw_msg iface =
   let sel_tm = 
      Unix.localtime (timestamp_of_line iface iface.left_selection)
   in
   let day_s = 
      Printf.sprintf "%s, %s %.2d" (string_of_tm_wday sel_tm.Unix.tm_wday)
         (full_string_of_tm_mon sel_tm.Unix.tm_mon) sel_tm.Unix.tm_mday
   in
   let day_time_s =
      match iface.selected_side with
      |Left ->
         day_s ^ " at " ^ (twelve_hour_string sel_tm)
      |Right ->
         day_s
   in
   for i = 0 to pred iface.scr.mw_lines do
      assert (wmove iface.scr.msg_win i 0);
      wclrtoeol iface.scr.msg_win
   done;
   (* draw the date stamp *)
   let acs = get_acs_codes () in
   wattron iface.scr.msg_win ((WA.color_pair 1) lor WA.bold lor WA.underline);
   trunc_mvwaddstr iface.scr.msg_win 0 0 iface.scr.mw_cols day_time_s;
   wattroff iface.scr.msg_win (WA.color_pair 1 lor WA.bold lor WA.underline);
   (* draw the current date *)
   let curr_tm = Unix.localtime (Unix.time ()) in
   wattron iface.scr.msg_win ((WA.color_pair 1) lor WA.bold);
   let s = 
      Printf.sprintf "Wyrd v%s          Currently: %s, %s %.2d at %s"
         Version.version (string_of_tm_wday curr_tm.Unix.tm_wday)
         (full_string_of_tm_mon curr_tm.Unix.tm_mon)
         curr_tm.Unix.tm_mday (twelve_hour_string curr_tm)
   in
   trunc_mvwaddstr iface.scr.msg_win (pred iface.scr.mw_lines) 0
      iface.scr.mw_cols s;
   wattroff iface.scr.msg_win ((WA.color_pair 1) lor WA.bold);
   (* draw the full MSG string, word wrapping as necessary *)
   let rec render_line lines i =
      match lines with
      |[] ->
         ()
      |line :: tail ->
         if i < pred iface.scr.mw_lines then begin
            assert (mvwaddstr iface.scr.msg_win i 5 line);
            render_line tail (succ i)
         end else
            ()
   in
   let message =
      match iface.selected_side with
      |Left ->
         begin match iface.timed_lineinfo.(iface.left_selection) with
         |None             -> ["(no reminder selected)"]
         |Some (_, _, msg) -> word_wrap msg (iface.scr.mw_cols - 5)
         end
      |Right ->
         begin match iface.untimed_lineinfo.(iface.right_selection) with
         |None             -> ["(no reminder selected)"]
         |Some (_, _, msg) -> word_wrap msg (iface.scr.mw_cols - 5)
         end
   in
   render_line message 1;
   assert (wnoutrefresh iface.scr.msg_win)


(* Draw a message in the error window.  If draw_cursor = true, then
 * add a blinking cursor at the end of the message. *)
let draw_error iface err draw_cursor =
   werase iface.scr.err_win;
   trunc_mvwaddstr iface.scr.err_win 0 0 iface.scr.ew_cols err;
   let len = String.length err in
   if draw_cursor && len <= pred iface.scr.ew_cols then begin
      wattron iface.scr.err_win WA.blink;
      assert (mvwaddch iface.scr.err_win 0 len (int_of_char '_'));
      wattroff iface.scr.err_win WA.blink
   end else
      ();
   assert (wnoutrefresh iface.scr.err_win)



(* arch-tag: DO_NOT_CHANGE_9ff0fd0c-6eb1-410f-8fcf-6dfcf94b346a *)
