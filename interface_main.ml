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

(* interface_main.ml
 * This file has the bulk of the implementation of the curses-based interface,
 * including the main program loop that grabs keypresses and processes them.
 * The screen rendering code is found in interface_draw.ml . *)

open Curses;;
open Printf;;
open Interface;;
open Interface_draw;;


exception Interrupt_exception;;



(*******************************************************************)
(* RESIZE HANDLER                                                  *)
(*******************************************************************)


(* create the (new) windows corresponding to the different areas of the screen *)
(* note: to get around curses issues with addch on the last character of the
 * last row, all windows are created one line too long.  The last lines are
 * never used. (Is this the best way to do things?) *)
let create_windows screen =
   let height, width  = get_size () in
   let cal_height     = 20
   and cal_width      = 30 in
   let msg_height     = 2 in
   let timed_height   = height - 1 - msg_height
   and timed_width    = width - cal_width
   and untimed_height = height - 1 - msg_height - cal_height
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
         msg_win      = newwin (succ msg_height) (width - 1) (1 + timed_height) 0;
         mw_lines     = msg_height;
         mw_cols      = width - 1
      }
      else
         (endwin ();
         failwith "Remic requires at least an 80 column window.")
   else
      (endwin (); 
      failwith "Remic requires at least a 24 line window.");;


(* resize the various windows to fit the new terminal size *)
(*
let resize_subwins scr =
   let height, width = get_size () in
   if height >= 24 then 
      if width >= 80 then
         begin
            scr.lines <- height;
            scr.cols <- width;
            begin match scr.help_win with
            |None ->
               scr.help_win <- Some (newwin (height - 2) 40 0 0)
            |Some win ->
               assert (wresize win (height - 2) 40);
            end;
            scr.hw_lines <- height - 2;
            scr.hw_cols <- 40;
            assert (wresize scr.stack_win (height - 2) 40);
            assert (mvwin scr.stack_win 0 40);
            scr.sw_lines <- height - 2;
            scr.sw_cols <- 40;
            assert (wresize scr.entry_win 2 80);
            assert (mvwin scr.entry_win (height - 2) 0);
            scr.ew_lines <- 2;
            scr.ew_cols <- 80
         end
      else if width >= 40 then
         (* only the stack window is provided *)
         begin
            scr.lines <- height;
            scr.cols <- width;
            begin match scr.help_win with
            |None ->
               ()
            |Some win ->
               assert (delwin win);
               scr.help_win <- None;
            end;
            scr.hw_lines <- 0;
            scr.hw_cols <- 0;
            assert (wresize scr.stack_win (height - 2) 40);
            assert (mvwin scr.stack_win 0 0);
            scr.sw_lines <- height - 2;
            scr.sw_cols <- 40;
            assert (wresize scr.entry_win 2 40);
            assert (mvwin scr.entry_win (height - 2) 0);
            scr.ew_lines <- 2;
            scr.ew_cols <- 40
         end
      else
         (endwin ();
         failwith "Orpie requires at least a 40 column window.")
   else
      (endwin (); 
      failwith "Orpie requires at least a 24 line window.");;
 *)


(* refresh the screen *)
let handle_refresh (iface : interface_state_t) =
   let _ = touchwin iface.scr.help_win in
   let _ = touchwin iface.scr.timed_win in
   let _ = touchwin iface.scr.calendar_win in
   let _ = touchwin iface.scr.untimed_win in
   let _ = touchwin iface.scr.msg_win in ()
   (* call window drawing code *)


(*
(* handle a terminal resize *)
let handle_resize (iface : interface_state_t) =
   (* reset ncurses *)
   endwin ();
   assert (refresh ());
   let rows, cols = get_size () in
   resize_subwins iface.scr;
   handle_refresh iface;;
 *)

(* Any time a new item is selected, the reminders
 * record needs updating and the screen need redrawing *)
let handle_selection_change new_iface reminders =
   let new_reminders = Remind.update_reminders reminders 
   (timestamp_of_line new_iface new_iface.left_selection) in
   draw_timed new_iface new_reminders.Remind.all_timed;
   draw_date_strip new_iface;
   draw_calendar new_iface new_reminders;
   (new_iface, new_reminders)


(* Handle keyboard input and update the display appropriately *)
let handle_keypress key iface reminders =
   if key = int_of_char 'j' then begin
      begin match iface.selected_side with
      |Left ->
         if iface.left_selection < pred iface.scr.tw_lines then begin
            let new_iface = {
               iface with left_selection = succ iface.left_selection
            } in
            handle_selection_change new_iface reminders
         end else begin
            let second_timestamp = timestamp_of_line iface 1 in
            let new_iface = {
               iface with top_timestamp = second_timestamp
            } in
            handle_selection_change new_iface reminders
         end
      |Right ->
           (iface, reminders)
      end
   end else if key = int_of_char 'k' then begin
      begin match iface.selected_side with
      |Left ->
         if iface.left_selection > 0 then begin
            let new_iface = {
               iface with left_selection = pred iface.left_selection
            } in
            handle_selection_change new_iface reminders
         end else begin
            let prev_timestamp = timestamp_of_line iface (-1) in
            let new_iface = {
               iface with top_timestamp = prev_timestamp
            } in
            handle_selection_change new_iface reminders
         end
      |Right ->
           (iface, reminders)
      end
   end else if key = int_of_char 'z' then begin
      let new_iface = 
         let curr_ts = timestamp_of_line iface iface.left_selection in
         let get_new_top delta = {
            curr_ts with Unix.tm_min = curr_ts.Unix.tm_min - 
                         (iface.left_selection * delta)
         } in
         match iface.zoom_level with
         |Hour ->
            let (_, new_top) = Unix.mktime (get_new_top 30) in {
               iface with top_timestamp = new_top;
                          zoom_level    = HalfHour
            }
         |HalfHour ->
            let (_, new_top) = Unix.mktime (get_new_top 15) in {
               iface with top_timestamp = new_top;
                          zoom_level    = QuarterHour
            }
         |QuarterHour ->
            let (_, new_top) = Unix.mktime (get_new_top 60) in {
               iface with top_timestamp = new_top;
                          zoom_level    = Hour
            }
      in
      draw_timed new_iface reminders.Remind.all_timed;
      draw_date_strip new_iface;
      (new_iface, reminders)
   end else
      (iface, reminders)



let rec do_main_loop (iface : interface_state_t) reminders =
   if iface.run_remic then begin
      assert (doupdate ());
      let key = wgetch iface.scr.help_win in
      (* using the ncurses SIGWINCH handler to catch window resize events *)
      let new_iface, new_reminders = 
         if key = Key.resize then
            (* handle_resize iface *)
            (iface, reminders)
         else
            handle_keypress key iface reminders
      in
      do_main_loop new_iface new_reminders
   end else
      () (* exit main loop *)



(* initialize the interface and begin the main loop *)
let run (iface : interface_state_t) =
   let reminders = Remind.create_three_month (iface.top_timestamp) in
   assert (keypad iface.scr.help_win true);
   draw_help iface;
   draw_date_strip iface;
   draw_timed iface reminders.Remind.all_timed;
   draw_calendar iface reminders;
   assert (doupdate ());
   do_main_loop iface reminders
        




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
