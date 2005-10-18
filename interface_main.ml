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

(* interface_main.ml
 * This file has the bulk of the implementation of the curses-based interface,
 * including the main program loop that grabs keypresses and processes them.
 * The screen rendering code is found in interface_draw.ml . *)

open Curses
open Printf
open Interface
open Interface_draw


exception Interrupt_exception
exception Template_undefined of string


type reminder_type_t = Timed | Untimed | General of int


(*******************************************************************)
(* RESIZE HANDLER                                                  *)
(*******************************************************************)


(* create the (new) windows corresponding to the different areas of the screen *)
(* note: to get around curses issues with addch on the last character of the
 * last row, all windows are created one line too long.  The last lines are
 * never used. (Is this the best way to do things?) *)
let create_windows screen =
   let height, width  = get_size () in
   let cal_height     = 10
   and cal_width      = 40 in
   let msg_height     = 6 in
   let err_height     = 1 in
   let timed_height   = height - 1 - msg_height - err_height
   and timed_width    = width - cal_width
   and untimed_height = height - 1 - msg_height - err_height - cal_height
   and untimed_width  = cal_width in
   if height >= 24 then 
      if width >= 80 then {
         stdscr       = screen;
         lines        = height;
         cols         = width;
         help_win     = newwin 2 width 0 0;
         hw_cols      = width;
         timed_win    = newwin (succ timed_height) timed_width 1 0;
         tw_lines     = timed_height;
         tw_cols      = timed_width;
         calendar_win = newwin (succ cal_height) cal_width 1 timed_width;
         cw_lines     = cal_height;
         cw_cols      = cal_width;
         untimed_win  = newwin (succ untimed_height) untimed_width (1 + cal_height)
                        timed_width;
         uw_lines     = untimed_height;
         uw_cols      = untimed_width;
         msg_win      = newwin (succ msg_height) width (1 + timed_height) 0;
         mw_lines     = msg_height;
         mw_cols      = width;
         err_win      = newwin err_height width (pred height) 0;
         ew_lines     = err_height;
         ew_cols      = pred width       (* don't print in last line of the last column *)
      }
      else
         (endwin ();
         failwith "Wyrd requires at least an 80 column window.")
   else
      (endwin (); 
      failwith "Wyrd requires at least a 24 line window.")


(* wresize all the child windows via wresize and mvwin *)
let resize_subwins iface =
   let height, width  = get_size () in
   let cal_height     = 10
   and cal_width      = 40 in
   let msg_height     = 5 in
   let err_height     = 1 in
   let timed_height   = height - 1 - msg_height - err_height
   and timed_width    = width - cal_width
   and untimed_height = height - 1 - msg_height - err_height - cal_height
   and untimed_width  = cal_width in
   let new_scr = 
      if height >= 24 then 
         if width >= 80 then {
            stdscr       = iface.scr.stdscr;
            lines        = height;
            cols         = width;
            help_win     = iface.scr.help_win;  (* subwindow pointers are unchanged *)
            hw_cols      = width;
            timed_win    = iface.scr.timed_win;
            tw_lines     = timed_height;
            tw_cols      = timed_width;
            calendar_win = iface.scr.calendar_win;
            cw_lines     = cal_height;
            cw_cols      = cal_width;
            untimed_win  = iface.scr.untimed_win;
            uw_lines     = untimed_height;
            uw_cols      = untimed_width;
            msg_win      = iface.scr.msg_win;
            mw_lines     = msg_height;
            mw_cols      = width;
            err_win      = iface.scr.err_win;
            ew_lines     = err_height;
            ew_cols      = pred width
         }
         else
            (endwin ();
            failwith "Wyrd requires at least an 80 column window.")
      else
         (endwin (); 
         failwith "Wyrd requires at least a 24 line window.")
   in
   assert (wresize new_scr.help_win 2 width);
   assert (wresize new_scr.timed_win (succ timed_height) timed_width);
   assert (wresize new_scr.calendar_win (succ cal_height) cal_width);
   assert (mvwin   new_scr.calendar_win 1 timed_width);
   assert (wresize new_scr.untimed_win (succ untimed_height) untimed_width);
   assert (mvwin   new_scr.untimed_win (1 + cal_height) timed_width);
   assert (wresize new_scr.msg_win (succ msg_height) width);
   assert (mvwin   new_scr.msg_win (1 + timed_height) 0);
   assert (wresize new_scr.err_win err_height width);
   assert (mvwin   new_scr.err_win (pred height) 0);
   new_scr


(* refresh the screen *)
let handle_refresh (iface : interface_state_t) reminders =
   let _ = touchwin iface.scr.help_win in
   let _ = touchwin iface.scr.timed_win in
   let _ = touchwin iface.scr.calendar_win in
   let _ = touchwin iface.scr.untimed_win in
   let _ = touchwin iface.scr.msg_win in 
   draw_help iface;
   let new_iface = draw_timed iface reminders.Remind.all_timed in
   draw_date_strip new_iface;
   draw_calendar new_iface reminders;
   let new_iface = draw_untimed new_iface reminders.Remind.curr_untimed in
   draw_error new_iface "" false;
   (new_iface, reminders)
   

(* handle a curses resize *)
let handle_resize (iface : interface_state_t) reminders =
   endwin ();
   let new_scr = resize_subwins iface in
   let resized_iface = {
      iface with scr = new_scr;
                 top_untimed = 0;
                 left_selection = if !Rcfile.center_cursor then (iface.scr.tw_lines / 2) - 1 else 1;
                 right_selection = 1;
                 timed_lineinfo = Array.make new_scr.tw_lines []
   } in
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   handle_refresh resized_iface reminders
   

(* Any time a new item is selected, the reminders
 * record needs updating and the screen need redrawing *)
let handle_selection_change iface reminders =
   let new_reminders = Remind.update_reminders reminders 
   (timestamp_of_line iface iface.left_selection) in
   let new_iface = draw_untimed iface new_reminders.Remind.curr_untimed in
   let new_iface = draw_timed new_iface new_reminders.Remind.all_timed in
   draw_date_strip new_iface;
   draw_calendar new_iface new_reminders;
   draw_error new_iface "" false;
   (new_iface, new_reminders)


(* Same as previous, but without calling draw_timed () or
 * draw_date_strip ().  Optimization for the scrolling case. *)
let handle_selection_change_scroll iface reminders =
   let new_reminders = Remind.update_reminders reminders 
   (timestamp_of_line iface iface.left_selection) in
   draw_calendar iface new_reminders;
   let new_iface = draw_untimed iface new_reminders.Remind.curr_untimed in
   draw_error new_iface "" false;
   (new_iface, new_reminders)


(* handle a "scroll down" event when the timed window is focused
 * and the center_cursor option is turned on *)
let handle_scrolldown_timed_center (iface : interface_state_t) reminders =
   let second_timestamp = timestamp_of_line iface 1 in
   let iface2 = {
      iface with top_timestamp = second_timestamp
   } in
   let (new_iface, new_reminders) =
      handle_selection_change_scroll iface2 reminders
   in
   (* use a curses scroll operation to shift up the timed window *)
   assert (wscrl new_iface.scr.timed_win 1);
   (* adjust lineinfo array to compensate for scrolling *)
   Array.blit iface.timed_lineinfo 1 iface.timed_lineinfo 0
      (pred (Array.length iface.timed_lineinfo));
   (* do a two-line update to recenter the cursor *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed
      (iface.left_selection - 1) 2 in
   (* draw in the new line at the bottom of the screen *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed
      (iface.scr.tw_lines - 1) 1 in
   draw_date_strip new_iface;
   (new_iface, new_reminders)



(* handle a "scroll down" event when the timed window is focused
 * and the center_cursor option is turned off *)
let handle_scrolldown_timed_nocenter (iface : interface_state_t) reminders =
   if iface.left_selection < pred iface.scr.tw_lines then begin
      (* case 1: only the cursor moves *)
      let iface2 = {
         iface with left_selection = succ iface.left_selection
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders
      in
      (* make a two-line update to erase and redraw the cursor *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 
         iface.left_selection 2 in
      (new_iface, new_reminders)
   end else begin
      (* case 2: the entire timed window scrolls *)
      let second_timestamp = timestamp_of_line iface 1 in
      let iface2 = {
         iface with top_timestamp = second_timestamp
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders
      in
      (* use a curses scroll operation to shift up the timed window *)
      assert (wscrl new_iface.scr.timed_win 1);
      (* adjust lineinfo array to compensate for scrolling *)
      Array.blit iface.timed_lineinfo 1 iface.timed_lineinfo 0
         (pred (Array.length iface.timed_lineinfo));
      (* do a two-line update to erase the cursor and draw in the
       * new line at the bottom of the screen *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed
         (iface.scr.tw_lines - 2) 2 in
      draw_date_strip new_iface;
      (new_iface, new_reminders)
   end



(* handle a "scroll down" event when the timed window is focused *)
let handle_scrolldown_timed (iface : interface_state_t) reminders =
   let iface = {
      iface with top_untimed = 0;
                 top_desc = 0;
                 right_selection = 1
   } in
   if !Rcfile.center_cursor then
      handle_scrolldown_timed_center iface reminders
   else
      handle_scrolldown_timed_nocenter iface reminders



(* handle a "scroll down" event when the untimed window is focused *)
let handle_scrolldown_untimed (iface : interface_state_t) reminders =
   if iface.right_selection < pred iface.scr.uw_lines then begin
      if iface.right_selection < iface.len_untimed - 
      iface.top_untimed then begin
         let new_iface = {
            iface with right_selection = succ iface.right_selection
         } in
         handle_selection_change new_iface reminders
      end else
         (iface, reminders)
   end else begin
      if iface.right_selection < iface.len_untimed - 
      iface.top_untimed then begin
         let new_iface = {
            iface with top_untimed = succ iface.top_untimed
         } in
         handle_selection_change new_iface reminders
      end else
         (iface, reminders)
   end



(* handle a "scroll up" event when the timed window is focused
 * and the center_cursor option is turned on *)
let handle_scrollup_timed_center (iface : interface_state_t) reminders =
   let prev_timestamp = timestamp_of_line iface (-1) in
   let iface2 = {
      iface with top_timestamp = prev_timestamp
   } in
   let (new_iface, new_reminders) = 
      handle_selection_change_scroll iface2 reminders
   in
   (* use a curses scroll operation to shift up the timed window *)
   assert (wscrl new_iface.scr.timed_win (-1));
   (* adjust lineinfo array to compensate for scrolling *)
   Array.blit iface.timed_lineinfo 0 iface.timed_lineinfo 1
      (pred (Array.length iface.timed_lineinfo));
   (* do a two-line update to recenter the cursor *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 
      iface.left_selection 2 in
   (* draw in the new top line of the schedule *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 0 1 in
   draw_date_strip new_iface;
   (new_iface, new_reminders)



(* handle a "scroll up" event when the timed window is focused
 * and the center_cursor option is turned off *)
let handle_scrollup_timed_nocenter (iface : interface_state_t) reminders =
   if iface.left_selection > 0 then begin
      (* case 1: only the cursor moves *)
      let iface2 = {
         iface with left_selection = pred iface.left_selection
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders
      in
      (* make a two-line update to erase and redraw the cursor *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 
         (pred iface.left_selection) 2 in
      (new_iface, new_reminders)
   end else begin
      (* case 2: the entire timed window scrolls *)
      let prev_timestamp = timestamp_of_line iface (-1) in
      let iface2 = {
         iface with top_timestamp = prev_timestamp
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders
      in
      (* use a curses scroll operation to shift up the timed window *)
      assert (wscrl new_iface.scr.timed_win (-1));
      (* adjust lineinfo array to compensate for scrolling *)
      Array.blit iface.timed_lineinfo 0 iface.timed_lineinfo 1
         (pred (Array.length iface.timed_lineinfo));
      (* do a two-line update to erase the cursor and draw in the
       * new line at the bottom of the screen *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 0 2 in
      draw_date_strip new_iface;
      (new_iface, new_reminders)
   end



(* handle a "scroll up" event when the timed window is focused *)
let handle_scrollup_timed (iface : interface_state_t) reminders =
   let iface = {
      iface with top_untimed = 0;
                 top_desc = 0;
                 right_selection = 1
   } in
   if !Rcfile.center_cursor then
      handle_scrollup_timed_center iface reminders
   else
      handle_scrollup_timed_nocenter iface reminders


(* handle a "scroll up" event when the untimed window is focused *)
let handle_scrollup_untimed (iface : interface_state_t) reminders =
   if iface.right_selection > 1 then
      let new_iface = {
         iface with right_selection = pred iface.right_selection
      } in
      handle_selection_change new_iface reminders
   else if iface.top_untimed > 0 then
      let new_iface = {
         iface with top_untimed = pred iface.top_untimed
      } in
      handle_selection_change new_iface reminders
   else
      (iface, reminders)


(* handle a jump to a different day *)
let handle_jump (iface : interface_state_t) reminders jump_func =
   let temp = Unix.localtime iface.top_timestamp in
   let next_tm = {
      temp with Unix.tm_mday = jump_func temp.Unix.tm_mday
   } in
   let (next_ts, _) = Unix.mktime next_tm in
   let new_iface = {
      iface with top_timestamp = next_ts;
                 top_desc = 0
   } in
   handle_selection_change new_iface reminders


(* handle a jump to the previous month *)
let handle_prev_month (iface : interface_state_t) reminders =
   let top = Unix.localtime iface.top_timestamp in
   let temp = {
      top with Unix.tm_mon = pred top.Unix.tm_mon
   } in
   let (prev_mon_ts, _) = Unix.mktime temp in
   let new_iface = {
      iface with top_timestamp = prev_mon_ts
   } in
   handle_selection_change new_iface reminders


(* handle a jump to the next month *)
let handle_next_month (iface : interface_state_t) reminders =
   let top = Unix.localtime iface.top_timestamp in
   let temp = {
      top with Unix.tm_mon = succ top.Unix.tm_mon
   } in
   let (next_mon_ts, _) = Unix.mktime temp in
   let new_iface = {
      iface with top_timestamp = next_mon_ts
   } in
   handle_selection_change new_iface reminders


(* handle a zoom keypress *)
let handle_zoom (iface : interface_state_t) reminders =
   let new_iface = 
      let curr_ts = timestamp_of_line iface iface.left_selection in
      let curr_tm = Unix.localtime curr_ts in
      let hour_tm = {
         curr_tm with Unix.tm_sec = 0;
                      Unix.tm_min = 0
      } in
      let (hour_ts, _) = Unix.mktime hour_tm in
      match iface.zoom_level with
      |Hour ->
         let new_top = curr_ts -. (60.0 *. 30.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       top_desc      = 0;
                       zoom_level    = HalfHour
         }
      |HalfHour ->
         let new_top = curr_ts -. (60.0 *. 15.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       top_desc      = 0;
                       zoom_level    = QuarterHour
         }
      |QuarterHour ->
         let new_top = hour_ts -. (60.0 *. 60.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       top_desc      = 0;
                       zoom_level    = Hour
         }
   in
   let new_iface = draw_timed new_iface reminders.Remind.all_timed in
   draw_date_strip new_iface;
   (new_iface, reminders)


(* handle switching window focus *)
let handle_switch_focus (iface : interface_state_t) reminders =
   let new_iface = 
      match iface.selected_side with
      |Left  -> {iface with selected_side = Right}
      |Right -> {iface with selected_side = Left}
   in
   handle_selection_change new_iface reminders


(* handle switching to the current timeslot *)
let handle_home (iface : interface_state_t) reminders =
   let curr_time = Unix.localtime ((Unix.time ()) -. (time_inc iface)) in
   let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level curr_time) in
   let new_iface = {
      iface with top_timestamp = 
                    if !Rcfile.center_cursor then
                       rounded_time -. (time_inc iface) *. (float_of_int ((iface.scr.tw_lines / 2) - 2))
                    else
                       rounded_time -. (time_inc iface) *. 1.;
                 top_desc        = 0;
                 selected_side   = Left;
                 left_selection  = if !Rcfile.center_cursor then (iface.scr.tw_lines / 2) - 1 else 2;
                 right_selection = 1
   } in
   handle_selection_change new_iface reminders


(* handle creation of a new timed or untimed reminder *)
let handle_new_reminder (iface : interface_state_t) reminders rem_type 
       remfile =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   let substitute template =
      let rec substitute_aux subst_list s =
         match subst_list with
         |[] ->
            s
         |(regex, replacement) :: tail ->
            let new_s = Str.global_replace regex replacement s in
            substitute_aux tail new_s
      in
      substitute_aux [
         (Str.regexp "%M", Utility.string_of_tm_mon tm.Unix.tm_mon);
         (Str.regexp "%d", string_of_int tm.Unix.tm_mday);
         (Str.regexp "%y", string_of_int (tm.Unix.tm_year + 1900));
         (Str.regexp "%h", Printf.sprintf "%.2d" tm.Unix.tm_hour);
         (Str.regexp "%m", Printf.sprintf "%.2d" tm.Unix.tm_min);
         (Str.regexp "%w", Utility.string_of_tm_wday tm.Unix.tm_wday)
      ] template
   in
   try
      let remline = 
         match rem_type with
         | Timed   -> substitute !Rcfile.timed_template
         | Untimed -> substitute !Rcfile.untimed_template
         | General x ->
            let template_status =
               match x with
               | 0 -> (!Rcfile.template0, "template0")
               | 1 -> (!Rcfile.template1, "template1")
               | 2 -> (!Rcfile.template2, "template2")
               | 3 -> (!Rcfile.template3, "template3")
               | 4 -> (!Rcfile.template4, "template4")
               | 5 -> (!Rcfile.template5, "template5")
               | 6 -> (!Rcfile.template6, "template6")
               | 7 -> (!Rcfile.template7, "template7")
               | 8 -> (!Rcfile.template8, "template8")
               | 9 -> (!Rcfile.template9, "template9")
               | _ -> failwith ("unexpected template number " ^ (string_of_int x) ^
                                " in handle_new_reminder")
            in
            begin match template_status with
            | (None, template_str) -> raise (Template_undefined template_str)
            | (Some t, _)          -> substitute t
            end
      in
      let remfile_channel = 
         open_out_gen [Open_append; Open_creat; Open_text] 416 remfile
      in
      output_string remfile_channel remline;
      close_out remfile_channel;
      let filename_sub = Str.regexp "%f" in
      let command = 
         Str.global_replace filename_sub remfile
            !Rcfile.edit_new_command
      in
      def_prog_mode ();
      endwin ();
      let _ = Unix.system command in 
      reset_prog_mode ();
      begin try
         assert (curs_set 0)
      with _ ->
         ()
      end;
      let r = Remind.create_three_month iface.top_timestamp in
      (* if the untimed list has been altered, change the focus to
       * the first element of the list *)
      let new_iface =
         if List.length r.Remind.curr_untimed <> 
            List.length reminders.Remind.curr_untimed then {
            iface with top_untimed = 0;
                       top_desc = 0;
                       right_selection = 1
            }
         else
            iface
      in
      handle_refresh new_iface r
   with Template_undefined template_str ->
      draw_error iface (template_str ^ " is undefined.") false;
      assert (doupdate ());
      (iface, reminders)


(* Search forward for the next occurrence of the current search regex.  
 *
 * First find the date of the occurrence, by matching on the output of 'remind
 * -n'.  For that date, recompute timed and untimed reminder lists.  Filter the
 * timed and untimed reminder lists to get only the entries falling on the
 * occurrence date.  Search through the filtered timed list first, and locate
 * the first entry that matches the regex (if any).  Then search through the 
 * filtered untimed list, and locate the first entry that matches the regex (if any).
 * If only one of the lists has a match, return that one; if both lists have a match,
 * return the one with the earlier timestamp.
 * Finally, reconfigure the iface record to highlight the matched entry.
 *
 * The saved search regex can be ignored by providing Some regex as the
 * override_regex parameter. *)
let handle_find_next (iface : interface_state_t) reminders override_regex =
   let search_regex =
      match override_regex with
      |None       -> iface.search_regex
      |Some regex -> regex
   in
   try
      (* Note: selected_ts is the timestamp of the next timeslot, minus one second.
       *       This ensures that searching begins immediately after the current
       *       selection. *)
      let selected_ts     = (timestamp_of_line iface (succ iface.left_selection)) -. 1.0 in
      let occurrence_time = Remind.find_next search_regex selected_ts in
      let new_reminders   = Remind.update_reminders reminders occurrence_time in
      let occurrence_tm   = Unix.localtime occurrence_time in
      let temp1 = {
         occurrence_tm with Unix.tm_sec  = 0;
                            Unix.tm_min  = 0;
                            Unix.tm_hour = 0
      } in
      let temp2 = {
         occurrence_tm with Unix.tm_sec  = 0;
                            Unix.tm_min  = 0;
                            Unix.tm_hour = 0;
                            Unix.tm_mday = succ occurrence_tm.Unix.tm_mday 
      } in
      let (day_start_ts, _) = Unix.mktime temp1 in
      let (day_end_ts, _)   = Unix.mktime temp2 in
      (* filter functions to determine reminders falling on the occurrence day *)
      let is_current_untimed (rem_ts, msg, _, _, _) =
         rem_ts >= day_start_ts && rem_ts < day_end_ts
      in
      let is_current_timed (rem_ts, msg, _, _, _, _) =
         rem_ts >= day_start_ts && rem_ts < day_end_ts
      in
      (* test the untimed reminders list for entries that match the regex *)
      let rec check_untimed untimed n timed_match =
         match untimed with
         |[] ->
            begin match timed_match with
            |None ->
               let _ = beep () in
               draw_error iface "search expression not found." false;
               assert (doupdate ());
               (iface, reminders)
            |Some (timed_match_ts, timed_match_iface) ->
               handle_selection_change timed_match_iface new_reminders
            end
         |(ts, msg, _, _, _) :: tail ->
            begin try
               if ts > selected_ts then
                  let _ = Str.search_forward search_regex msg 0 in
                  let tm = Unix.localtime ts in
                  let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level tm) in
                  let new_iface =
                     (* take care of highlighting the correct untimed reminder *)
                     if n >= iface.scr.uw_lines then 
                        if !Rcfile.center_cursor then {
                           iface with top_timestamp   = rounded_time -. (time_inc iface) *. 
                                                        (float_of_int (iface.scr.tw_lines / 2 - 1));
                                      top_untimed     = n - iface.scr.uw_lines + 1;
                                      top_desc        = 0;
                                      left_selection  = (iface.scr.tw_lines / 2) - 1;
                                      right_selection = pred iface.scr.uw_lines;
                                      selected_side   = Right
                        } else {
                           iface with top_timestamp   = rounded_time;
                                      top_untimed     = n - iface.scr.uw_lines + 1;
                                      top_desc        = 0;
                                      left_selection  = 0;
                                      right_selection = pred iface.scr.uw_lines;
                                      selected_side   = Right
                        }
                     else 
                        if !Rcfile.center_cursor then {
                           iface with top_timestamp   = rounded_time -. (time_inc iface) *. 
                                                        (float_of_int (iface.scr.tw_lines / 2 - 1));
                                      top_untimed     = 0;
                                      top_desc        = 0;
                                      left_selection  = (iface.scr.tw_lines / 2) - 1 ;
                                      right_selection = n;
                                      selected_side   = Right
                        } else {
                           iface with top_timestamp   = rounded_time;
                                      top_untimed     = 0;
                                      top_desc        = 0;
                                      left_selection  = 0;
                                      right_selection = n;
                                      selected_side   = Right
                        }
                  in
                  (* choose between the timed reminder match and the untimed reminder match *)
                  begin match timed_match with
                  |None ->
                     handle_selection_change new_iface new_reminders
                  |Some (timed_match_ts, timed_match_iface) ->
                     if timed_match_ts < ts then
                        handle_selection_change timed_match_iface new_reminders
                     else
                        handle_selection_change new_iface new_reminders
                  end
               else
                  raise Not_found
            with Not_found ->
               check_untimed tail (succ n) timed_match
            end
      in
      (* test the timed reminders list for entries that match the regex *)
      let rec check_timed timed =
         match timed with
         |[] ->
            let today_untimed = 
               List.filter is_current_untimed new_reminders.Remind.curr_untimed
            in
            check_untimed today_untimed 1 None
         |(ts, _, msg, _, _, _) :: tail ->
            begin try
               if ts > selected_ts then
                  let _ = Str.search_forward search_regex msg 0 in
                  let tm = Unix.localtime ts in
                  let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level tm) in
                  let new_iface = 
                     if !Rcfile.center_cursor then {
                        iface with top_timestamp   = rounded_time -. (time_inc iface) *. 
                                                     (float_of_int (iface.scr.tw_lines / 2 - 1));
                                   top_desc        = 0;
                                   left_selection  = (iface.scr.tw_lines / 2) - 1;
                                   right_selection = 1;
                                   selected_side   = Left
                        } 
                     else {
                        iface with top_timestamp   = rounded_time -. (time_inc iface) *. 2.;
                                   top_desc        = 0;
                                   left_selection  = 2;
                                   right_selection = 1;
                                   selected_side   = Left
                     } 
                  in
                  let today_untimed = 
                     List.filter is_current_untimed new_reminders.Remind.curr_untimed
                  in
                  check_untimed today_untimed 1 (Some (ts, new_iface))
               else
                  raise Not_found
            with Not_found ->
               check_timed tail
            end
      in
      let merged_rem = Remind.merge_timed new_reminders.Remind.curr_timed in
      check_timed (List.filter is_current_timed merged_rem)
   with Not_found ->
      let _ = beep () in
      draw_error iface "search expression not found." false;
      (iface, reminders)


(* Begin entry of a search string *)
let handle_begin_search (iface : interface_state_t) reminders =
   let new_iface = {
      iface with is_entering_search = true
   } in
   draw_error iface "search expression: " true;
   (new_iface, reminders)


(* Find the next reminder after the current selection.
 * This algorithm is a dirty hack, but actually seems to work:
 * 1) If an untimed reminder is selected, and if it is not the last
 *    untimed reminder in the list, then highlight the next untimed reminder
 * 2) Otherwise, run find_next with a regexp that matches anything. *)
let handle_next_reminder (iface : interface_state_t) reminders =
   if iface.selected_side = Right && 
   iface.right_selection < iface.len_untimed - iface.top_untimed then 
      handle_scrolldown_untimed iface reminders
   else
      handle_find_next iface reminders (Some (Str.regexp ""))


(* View the reminders for the selected date using 'less'.
 * If 'trigger_all' is true, then all nonexpired reminders are
 * triggered. *)
let handle_view_reminders (iface : interface_state_t) reminders trigger_all =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   let rem_date_str = (Utility.string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let partial_command =
      if trigger_all then
         "remind -q -g -t "
      else
         "remind -q -g "
   in
   let command = partial_command ^ !Rcfile.reminders_file ^ " " ^
   rem_date_str ^ " | less -c" in
   def_prog_mode ();
   endwin ();
   let _ = Unix.system command in 
   reset_prog_mode ();
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   handle_refresh iface reminders



(* View Remind's formatted calendar output for the selected date.  If
 * 'week_only' is true, then create a week calendar, otherwise a month
 * calendar. *)
let handle_view_calendar (iface : interface_state_t) reminders week_only =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   let rem_date_str = (Utility.string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let partial_command = 
      if week_only then
         Printf.sprintf "remind -c+1 -w%d " iface.scr.cols
      else
         Printf.sprintf "remind -c -w%d " iface.scr.cols
   in
   let time_option = if !Rcfile.description_12_hour then "-b0 " else "-b1 " in
   let weekday_option = if !Rcfile.week_starts_monday then "-m " else "" in
   let command = 
      partial_command ^ time_option ^ weekday_option ^ 
      !Rcfile.reminders_file ^ " " ^ rem_date_str ^ " | less -c" 
   in
   def_prog_mode ();
   endwin ();
   let _ = Unix.system command in 
   reset_prog_mode ();
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   handle_refresh iface reminders


let handle_view_keybindings (iface : interface_state_t) reminders =
   let bindings = ref [] in
   let find_binding commandstr operation =
      match operation with
      |Rcfile.CommandOp command ->
         begin try
            let key_str = Hashtbl.find Rcfile.table_command_key command in
            bindings := (Printf.sprintf "%-30s%-20s\n" commandstr key_str) :: !bindings
         with Not_found ->
            bindings := (Printf.sprintf "%-30s\n" commandstr) :: !bindings
         end
      |Rcfile.EntryOp entry ->
         begin try
            let key_str = Hashtbl.find Rcfile.table_entry_key entry in
            bindings := (Printf.sprintf "%-30s%-20s\n" commandstr key_str) :: !bindings
         with Not_found ->
            bindings := (Printf.sprintf "%-30s\n" commandstr) :: !bindings
         end
   in
   Hashtbl.iter find_binding Rcfile.table_commandstr_command;
   let sorted_list = List.fast_sort Pervasives.compare !bindings in
   let out_channel = open_out Rcfile.tmpfile in
   List.iter (output_string out_channel) sorted_list;
   close_out out_channel;
   def_prog_mode ();
   endwin ();
   let _ = Unix.system ("less " ^ Rcfile.tmpfile) in 
   reset_prog_mode ();
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   handle_refresh iface reminders


(* Handle scrolling down during selection dialog loop *)
let handle_selection_dialog_scrolldown (elements : string list) 
       (selection : int) (top : int) =
   if selection < pred (List.length elements) then
      let lines, cols = get_size () in
      let new_selection = succ selection in
      let new_top =
         if new_selection - top >= pred lines then 
            succ top 
         else 
            top
      in
      (new_selection, new_top)
   else
      (selection, top)


(* Handle scrolling up during selection dialog loop *)
let handle_selection_dialog_scrollup (elements : string list) 
       (selection : int) (top : int) =
   if selection > 0 then
      let new_selection = pred selection in
      let new_top =
         if new_selection < top then 
            pred top 
         else 
            top
      in
      (new_selection, new_top)
   else
      (selection, top)


(* Begin a selection dialog loop *)
let do_selection_dialog (iface : interface_state_t) (title : string) 
       (elements : string list) =
   let init_selection = 0
   and init_top       = 0 in
   let rec selection_loop (selection, top) =
      draw_selection_dialog iface title elements selection top;
      let key = wgetch iface.scr.help_win in
      (* trap the wgetch timeout *)
      if key <> ~-1 then
         try
            match Rcfile.command_of_key key with
            |Rcfile.ScrollDown ->
               selection_loop (handle_selection_dialog_scrolldown elements selection top)
            |Rcfile.ScrollUp ->
               selection_loop (handle_selection_dialog_scrollup elements selection top)
            |Rcfile.Edit ->
               List.nth elements selection
            |_ ->
               let _ = beep () in
               selection_loop (selection, top)
         with Not_found ->
            let _ = beep () in
            selection_loop (selection, top)
      else
         selection_loop (selection, top)
   in
   selection_loop (init_selection, init_top)


(* handle editing the selected reminder *)
let handle_edit (iface : interface_state_t) reminders =
   let adjust_s len s =
      let pad = s ^ (String.make len ' ') in
      Str.string_before pad len
   in
   let fl =
      match iface.selected_side with
      |Left  -> 
         begin match iface.timed_lineinfo.(iface.left_selection) with
         | [] -> 
            None
         | (f, l, t, m, s) :: [] -> 
            Some (f, l, m)
         | rem_list -> 
              let sorted_rem_list = List.fast_sort sort_lineinfo rem_list in
              let get_msg (_, _, time, msg, _) = (adjust_s 16 time) ^ msg in
              let msg_list = List.rev_map get_msg sorted_rem_list in
              let selected_msg =
                 do_selection_dialog iface "Choose a reminder to edit" msg_list
              in
              let test_msg_match (_, _, time, msg, _) = 
                 ((adjust_s 16 time) ^ msg) = selected_msg in
              let (f, l, t, m, s) = (List.find test_msg_match sorted_rem_list) in
              Some (f, l, m)
         end
      |Right -> iface.untimed_lineinfo.(iface.right_selection)
   in
   begin match fl with
   |None ->
      if iface.selected_side = Left then
         handle_new_reminder iface reminders Timed
         (Utility.expand_file !Rcfile.reminders_file)
      else
         handle_new_reminder iface reminders Untimed
         (Utility.expand_file !Rcfile.reminders_file)
   |Some (filename, line_num, msg) -> 
      let filename_sub = Str.regexp "%f" in
      let lineno_sub   = Str.regexp "%n" in
      let command_partial = 
         Str.global_replace filename_sub filename !Rcfile.edit_old_command
      in
      let command = Str.global_replace lineno_sub line_num command_partial in
      def_prog_mode ();
      endwin ();
      let _ = Unix.system command in 
      reset_prog_mode ();
      begin try
         assert (curs_set 0)
      with _ ->
         ()
      end;
      let r = Remind.create_three_month iface.top_timestamp in
      (* if the untimed list has been altered, change the focus
       * to the first element *)
      let new_iface =
         if List.length r.Remind.curr_untimed <> 
            List.length reminders.Remind.curr_untimed then {
            iface with top_untimed = 0;
                       top_desc = 0;
                       right_selection = 1
            }
         else
            iface
      in
      handle_refresh new_iface r
   end



(* handle free editing of the reminders file *)
let handle_edit_any (iface : interface_state_t) reminders remfile =
   let filename_sub = Str.regexp "%f" in
   let command = 
      Str.global_replace filename_sub remfile !Rcfile.edit_any_command
   in
   def_prog_mode ();
   endwin ();
   let _ = Unix.system command in 
   reset_prog_mode ();
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   let r = Remind.create_three_month iface.top_timestamp in
   (* if the untimed list has been altered, change the focus
    * to the first element *)
   let new_iface =
      if List.length r.Remind.curr_untimed <> 
         List.length reminders.Remind.curr_untimed then {
         iface with top_untimed = 0;
                    top_desc = 0;
                    right_selection = 1
         }
      else
         iface
   in
   handle_refresh new_iface r



(* handle scrolling the description window up *)
let handle_scroll_desc_up iface reminders =
   let new_iface = {iface with top_desc = succ iface.top_desc} in
   handle_refresh new_iface reminders


(* handle scrolling the description window down *)
let handle_scroll_desc_down iface reminders =
   let top = max 0 (pred iface.top_desc) in
   let new_iface = {iface with top_desc = top} in
   handle_refresh new_iface reminders


(* handle keyboard input and update the display appropriately *)
let handle_keypress key (iface : interface_state_t) reminders =
   if not iface.is_entering_search then begin
      try
         match Rcfile.command_of_key key with
         |Rcfile.ScrollDown ->
            begin match iface.selected_side with
            |Left  -> handle_scrolldown_timed iface reminders
            |Right -> handle_scrolldown_untimed iface reminders
            end
         |Rcfile.ScrollUp ->
            begin match iface.selected_side with
            |Left  -> handle_scrollup_timed iface reminders
            |Right -> handle_scrollup_untimed iface reminders
            end
         |Rcfile.NextDay ->
            handle_jump iface reminders succ
         |Rcfile.PrevDay ->
            handle_jump iface reminders pred
         |Rcfile.NextWeek ->
            let next_week i = i + 7 in
            handle_jump iface reminders next_week
         |Rcfile.PrevWeek ->
            let prev_week i = i - 7 in
            handle_jump iface reminders prev_week
         |Rcfile.NextMonth ->
            handle_next_month iface reminders
         |Rcfile.PrevMonth ->
            handle_prev_month iface reminders
         |Rcfile.Zoom ->
            handle_zoom iface reminders
         |Rcfile.SwitchWindow ->
            handle_switch_focus iface reminders
         |Rcfile.Home ->
            handle_home iface reminders
         |Rcfile.Edit ->
            handle_edit iface reminders
         |Rcfile.EditAny ->
            let remfile = 
               do_selection_dialog iface "Choose a reminders file"
               (Remind.get_included_remfiles ())
            in
            handle_edit_any iface reminders remfile
         |Rcfile.ScrollDescUp ->
            handle_scroll_desc_up iface reminders
         |Rcfile.ScrollDescDown ->
            handle_scroll_desc_down iface reminders
         |Rcfile.NewTimed ->
            handle_new_reminder iface reminders Timed
            (Utility.expand_file !Rcfile.reminders_file)
         |Rcfile.NewTimedDialog ->
            let remfile = 
               do_selection_dialog iface "Choose a reminders file"
               (Remind.get_included_remfiles ())
            in
            handle_new_reminder iface reminders Timed remfile
         |Rcfile.NewUntimed ->
            handle_new_reminder iface reminders Untimed
            (Utility.expand_file !Rcfile.reminders_file)
         |Rcfile.NewUntimedDialog ->
            let remfile = 
               do_selection_dialog iface "Choose a reminders file"
               (Remind.get_included_remfiles ())
            in
            handle_new_reminder iface reminders Untimed remfile
         |Rcfile.NewGenReminder x ->
            handle_new_reminder iface reminders (General x)
            (Utility.expand_file !Rcfile.reminders_file)
         |Rcfile.NewGenReminderDialog x ->
            let remfile = 
               do_selection_dialog iface "Choose a reminders file"
               (Remind.get_included_remfiles ())
            in
            handle_new_reminder iface reminders (General x) remfile
         |Rcfile.SearchNext ->
            handle_find_next iface reminders None
         |Rcfile.BeginSearch ->
            handle_begin_search iface reminders
         |Rcfile.NextReminder ->
            handle_next_reminder iface reminders
         |Rcfile.ViewReminders ->
            handle_view_reminders iface reminders false
         |Rcfile.ViewAllReminders ->
            handle_view_reminders iface reminders true
         |Rcfile.ViewWeek ->
            handle_view_calendar iface reminders true
         |Rcfile.ViewMonth ->
            handle_view_calendar iface reminders false
         |Rcfile.ViewKeybindings ->
            handle_view_keybindings iface reminders
         |Rcfile.Refresh ->
            (* NOTE: I'm not sure why the endwin call is necessary here,
             * but I'm having problems getting a true full-screen refresh
             * without it. *)
            def_prog_mode ();
            endwin ();
            reset_prog_mode ();
            begin try
               assert (curs_set 0)
            with _ ->
               ()
            end;
            let i = draw_msg iface in
            handle_refresh i reminders
         |Rcfile.Quit ->
            let new_iface = {iface with run_wyrd = false} in
            (new_iface, reminders)
      with Not_found ->
         let _ = beep () in
         draw_error iface "key is not bound." false;
         assert (doupdate ());
         (iface, reminders)
   end else begin
      (* user is entering a search string *)
      try
         begin match Rcfile.entry_of_key key with
         |Rcfile.EntryComplete ->
            begin try
               let new_iface = {
                  iface with search_regex = Str.regexp_case_fold iface.search_input;
                             search_input = "";
                             is_entering_search = false
               } in
               handle_find_next new_iface reminders None
            with Failure err ->
               let new_iface = {
                  iface with search_input = "";
                             is_entering_search = false
               } in
               draw_error new_iface ("syntax error in search string: " ^ err) false;
               assert (doupdate ());
               (new_iface, reminders)
            end
         |Rcfile.EntryBackspace ->
            let len = String.length iface.search_input in
            if len > 0 then 
               let new_iface = {
               iface with search_input = 
                           Str.string_before iface.search_input (pred len)
               } in
               draw_error iface ("search expression: " ^ new_iface.search_input) true;
               (new_iface, reminders)
            else
               let _ = beep () in
               (iface, reminders)
         |Rcfile.EntryExit ->
            let new_iface = {
               iface with search_input = "";
                          is_entering_search = false
            } in
            draw_error new_iface "search cancelled." false;
            (new_iface, reminders)
         end
      with Not_found ->
         begin try
            (* only printable characters are accepted for search strings *)
            if key >= 32 && key <= 126 then begin
               let c = char_of_int key in
               let new_iface = {
                  iface with search_input = iface.search_input ^ (String.make 1 c)
               } in
               draw_error new_iface ("search expression: " ^ new_iface.search_input) true;
               (new_iface, reminders)
            end else
               failwith "cannot search for unprintable characters"
         with Failure _ ->
            let _ = beep () in
            (iface, reminders)
         end
   end




let rec do_main_loop (iface : interface_state_t) reminders last_update =
   if iface.run_wyrd then begin
      (* refresh the msg window (which contains a clock)
       * every wgetch timeout cycle *)
      let iface = draw_msg iface in
      assert (doupdate ());
      let key = wgetch iface.scr.help_win in
      let new_iface, new_reminders = 
         (* key = -1 is ncurses wgetch timeout error *)
         if key <> ~- 1 then
            if key = Key.resize then
               handle_resize iface reminders
            else
               handle_keypress key iface reminders
         else
            (iface, reminders)
      in
      let curr_time = Unix.time () in
      (* poll remind(1) every 5 minutes and update display *)
      if curr_time -. last_update > 300.0 then begin
         let r = Remind.create_three_month new_iface.top_timestamp in
         (* if the untimed list has been altered, change the focus to
          * the timed window *)
         let new_iface =
            if List.length r.Remind.curr_untimed <> 
               List.length reminders.Remind.curr_untimed then {
               new_iface with selected_side = Left;
                              top_untimed = 0;
                              right_selection = 1
               }
            else
               new_iface
         in
         let (iface2, reminders2) = handle_refresh new_iface r in
         do_main_loop iface2 reminders2 curr_time
      end else
         do_main_loop new_iface new_reminders last_update
   end else
      endwin () (* exit main loop *)



(* initialize the interface and begin the main loop *)
let run (iface : interface_state_t) =
   scrollok iface.scr.timed_win true;
   let reminders = Remind.create_three_month (iface.top_timestamp) in
   assert (keypad iface.scr.help_win true);
   draw_help iface;
   draw_date_strip iface;
   let new_iface = draw_timed iface reminders.Remind.all_timed in
   draw_calendar new_iface reminders;
   let new_iface = draw_untimed new_iface reminders.Remind.curr_untimed in
   let new_iface = draw_msg new_iface in
   draw_error new_iface "" false;
   assert (doupdate ());
   do_main_loop new_iface reminders (Unix.time ())
        




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
