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

open Curses;;
open Printf;;
open Interface;;
open Interface_draw;;


exception Interrupt_exception;;

type reminder_type_t = Timed | Untimed


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
   let msg_height     = 5 in
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
      failwith "Wyrd requires at least a 24 line window.");;


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
   (* draw_msg new_iface; *)
   draw_error new_iface "" false;
   (new_iface, reminders)
   


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
let handle_selection_change iface reminders =
   let new_reminders = Remind.update_reminders reminders 
   (timestamp_of_line iface iface.left_selection) in
   let new_iface = draw_timed iface new_reminders.Remind.all_timed in
   draw_date_strip new_iface;
   draw_calendar new_iface new_reminders;
   let new_iface = draw_untimed new_iface new_reminders.Remind.curr_untimed in
   (* draw_msg new_iface; *)
   draw_error new_iface "" false;
   (new_iface, new_reminders)



(* handle a "scroll down" event when the timed window is focused *)
let handle_scrolldown_timed (iface : interface_state_t) reminders =
   let iface = {
      iface with top_untimed = 0;
                 right_selection = 1
   } in
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



(* handle a "scroll up" event when the timed window is focused *)
let handle_scrollup_timed (iface : interface_state_t) reminders =
   let iface = {
      iface with top_untimed = 0;
                 right_selection = 1
   } in
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
      iface with top_timestamp = next_ts
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
                       zoom_level    = HalfHour
         }
      |HalfHour ->
         let new_top = curr_ts -. (60.0 *. 15.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       zoom_level    = QuarterHour
         }
      |QuarterHour ->
         let new_top = hour_ts -. (60.0 *. 60.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       zoom_level    = Hour
         }
   in
   let final_iface = draw_timed new_iface reminders.Remind.all_timed in
   draw_date_strip final_iface;
   (final_iface, reminders)


(* handle switching window focus *)
let handle_switch_focus (iface : interface_state_t) reminders =
   if iface.len_untimed > 0 then
      let new_iface = 
         match iface.selected_side with
         |Left  -> {iface with selected_side = Right}
         |Right -> {iface with selected_side = Left}
      in
      handle_selection_change new_iface reminders
   else
      (iface, reminders)


(* handle switching to the current timeslot *)
let handle_home (iface : interface_state_t) reminders =
   let curr_time = Unix.localtime ((Unix.time ()) -. (time_inc iface)) in
   let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level curr_time) in
   let new_iface = {
      iface with top_timestamp  = rounded_time;
                 selected_side  = Left;
                 left_selection = 1
   } in
   handle_selection_change new_iface reminders


(* handle editing the selected reminder *)
let handle_edit (iface : interface_state_t) reminders =
   let fl =
      match iface.selected_side with
      |Left  -> iface.timed_lineinfo.(iface.left_selection)
      |Right -> iface.untimed_lineinfo.(iface.right_selection)
   in
   begin match fl with
   |None ->
      (iface, reminders)
   |Some (filename, line_num, msg) -> 
      let filename_sub = Str.regexp "%f" in
      let lineno_sub   = Str.regexp "%n" in
      let command_partial = 
         Str.global_replace filename_sub filename !Rcfile.edit_old_command
      in
      let command = Str.global_replace lineno_sub line_num command_partial in
      endwin();
      let _ = Unix.system command in 
      assert (curs_set 0);
      let r = Remind.create_three_month iface.top_timestamp in
      (* if the untimed list has been altered, change the focus to
       * the timed window *)
      let new_iface =
         if List.length r.Remind.curr_untimed <> 
            List.length reminders.Remind.curr_untimed then {
            iface with selected_side = Left;
                       top_untimed = 0;
                       right_selection = 1
            }
         else
            iface
      in
      handle_refresh new_iface r
   end


(* handle creation of a new timed or untimed reminder *)
let handle_new_reminder (iface : interface_state_t) reminders rem_type =
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
         (Str.regexp "%M", Remind.string_of_tm_mon tm.Unix.tm_mon);
         (Str.regexp "%d", string_of_int tm.Unix.tm_mday);
         (Str.regexp "%y", string_of_int (tm.Unix.tm_year + 1900));
         (Str.regexp "%h", Printf.sprintf "%.2d" tm.Unix.tm_hour);
         (Str.regexp "%m", Printf.sprintf "%.2d" tm.Unix.tm_min);
         (Str.regexp "%w", Remind.string_of_tm_wday tm.Unix.tm_wday)
      ] template
   in
   let remline = 
      match rem_type with
      | Timed   -> substitute !Rcfile.timed_template
      | Untimed -> substitute !Rcfile.untimed_template
   in
   let remfile_channel = open_out_gen [Open_append; Open_creat; Open_text] 416 
   (Utility.expand_file !Rcfile.reminders_file) in
   output_string remfile_channel remline;
   close_out remfile_channel;
   let filename_sub = Str.regexp "%f" in
   let command = 
      Str.global_replace filename_sub !Rcfile.reminders_file 
         !Rcfile.edit_new_command
   in
   endwin();
   let _ = Unix.system command in 
   assert (curs_set 0);
   let r = Remind.create_three_month iface.top_timestamp in
   (* if the untimed list has been altered, change the focus to
    * the timed window *)
   let new_iface =
      if List.length r.Remind.curr_untimed <> 
         List.length reminders.Remind.curr_untimed then {
         iface with selected_side = Left;
                    top_untimed = 0;
                    right_selection = 1
         }
      else
         iface
   in
   handle_refresh new_iface r


(* Search forward for the next occurrence of the current search regex.  
 *
 * First find the date of the occurrence, by matching on the output of 'remind
 * -n'.  For that date, recompute timed and untimed reminder lists.  Filter the
 * timed and untimed reminder lists to get only the entries falling on the
 * occurrence date.  Search through the filtered timed list first, then the
 * filtered untimed list, and locate the first entry that matches the regex.
 * Finally, reconfigure the iface record to highlight the matched entry. *)
let handle_find_next (iface : interface_state_t) reminders =
   try
      (* Note: selected_ts is the timestamp of the next timeslot, minus one second.
       *       This ensures that searching begins immediately after the current
       *       selection. *)
      let selected_ts     = (timestamp_of_line iface (succ iface.left_selection)) -. 1.0 in
      let occurrence_time = Remind.find_next iface.search_regex selected_ts in
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
      let is_current_untimed (rem_ts, msg, _, _) =
         rem_ts >= day_start_ts && rem_ts < day_end_ts
      in
      let is_current_timed (rem_ts, msg, _, _, _) =
         rem_ts >= day_start_ts && rem_ts < day_end_ts
      in
      (* test the untimed reminders list for entries that match the regex *)
      let rec check_untimed untimed n =
         match untimed with
         |[] ->
            let _ = beep () in
            draw_error iface "search expression not found." false;
            assert (doupdate ());
            (iface, reminders)
         |(ts, msg, _, _) :: tail ->
            begin try
               if ts > selected_ts then
                  let _ = Str.search_forward iface.search_regex msg 0 in
                  let tm = Unix.localtime ts in
                  let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level tm) in
                  let new_iface =
                     (* take care of highlighting the correct untimed reminder *)
                     if n >= iface.scr.uw_lines then {
                        iface with top_timestamp   = rounded_time;
                                   top_untimed     = n - iface.scr.uw_lines + 1;
                                   left_selection  = 0;
                                   right_selection = pred iface.scr.uw_lines;
                                   selected_side   = Right
                     } else {
                        iface with top_timestamp   = rounded_time;
                                   top_untimed     = 0;
                                   left_selection  = 0;
                                   right_selection = n;
                                   selected_side   = Right
                     }
                  in
                  handle_selection_change new_iface new_reminders
               else
                  raise Not_found
            with Not_found ->
               check_untimed tail (succ n)
            end
      in
      (* test the timed reminders list for entries that match the regex *)
      let rec check_timed timed =
         match timed with
         |[] ->
            let today_untimed = 
               List.filter is_current_untimed new_reminders.Remind.curr_untimed
            in
            check_untimed today_untimed 1
         |(ts, _, msg, _, _) :: tail ->
            begin try
               if ts > selected_ts then
                  let _ = Str.search_forward iface.search_regex msg 0 in
                  let tm = Unix.localtime ts in
                  let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level tm) in
                  let new_iface = {
                     iface with top_timestamp   = rounded_time;
                                left_selection  = 0;
                                right_selection = 1;
                                selected_side   = Left
                  } in
                  handle_selection_change new_iface new_reminders
               else
                  raise Not_found
            with Not_found ->
               check_timed tail
            end
      in
      check_timed (List.filter is_current_timed new_reminders.Remind.curr_timed)
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


(* Handle keyboard input and update the display appropriately *)
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
         |Rcfile.NewTimed ->
            handle_new_reminder iface reminders Timed
         |Rcfile.NewUntimed ->
            handle_new_reminder iface reminders Untimed
         |Rcfile.SearchNext ->
            handle_find_next iface reminders
         |Rcfile.BeginSearch ->
            handle_begin_search iface reminders
         |Rcfile.Quit ->
            let new_iface = {iface with run_wyrd = false} in
            (new_iface, reminders)
      with Not_found ->
         (iface, reminders)
   end else begin
      (* user is entering a search string *)
      try
         begin match Rcfile.entry_of_key key with
         |Rcfile.EntryComplete ->
            let new_iface = {
               iface with search_regex = Str.regexp_case_fold iface.search_input;
                          search_input = "";
                          is_entering_search = false
            } in
            handle_find_next new_iface reminders
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
            let c = char_of_int key in
            let new_iface = {
               iface with search_input = iface.search_input ^ (String.make 1 c)
            } in
            draw_error new_iface ("search expression: " ^ new_iface.search_input) true;
            (new_iface, reminders)
         with Failure _ ->
            let _ = beep () in
            (iface, reminders)
         end
   end




let rec do_main_loop (iface : interface_state_t) reminders last_update =
   if iface.run_wyrd then begin
      (* refresh the msg window (which contains a clock)
       * every wgetch timeout cycle *)
      draw_msg iface;
      assert (doupdate ());
      let key = wgetch iface.scr.help_win in
      (* using the ncurses SIGWINCH handler to catch window resize events *)
      let new_iface, new_reminders = 
         if key = Key.resize then begin
            (* handle_resize iface *)
            Printf.fprintf stderr "Error: window resize not handled yet.\n";
            flush stderr;
            (iface, reminders)
         end else
            handle_keypress key iface reminders
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
   let reminders = Remind.create_three_month (iface.top_timestamp) in
   assert (keypad iface.scr.help_win true);
   draw_help iface;
   draw_date_strip iface;
   let new_iface = draw_timed iface reminders.Remind.all_timed in
   draw_calendar new_iface reminders;
   let new_iface = draw_untimed new_iface reminders.Remind.curr_untimed in
   draw_msg new_iface;
   draw_error new_iface "" false;
   assert (doupdate ());
   do_main_loop new_iface reminders (Unix.time ())
        




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
