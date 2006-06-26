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

(* interface.ml
 * This file defines the data structure (a record) that stores the
 * interface state. *)


open Curses;;
open Rcfile;;

exception Not_handled;;

(* help_win is provided as an option, because it may be dropped if
 * the screen width is too small *)
type screen_t = {
   stdscr       : window;
   lines        : int;
   cols         : int;
   help_win     : window;
   hw_cols      : int;
   timed_win    : window;
   tw_lines     : int;
   tw_cols      : int;
   calendar_win : window;
   cw_lines     : int;
   cw_cols      : int;
   untimed_win  : window;
   uw_lines     : int;
   uw_cols      : int;
   msg_win      : window;
   mw_lines     : int;
   mw_cols      : int;
   err_win      : window;
   ew_lines     : int;
   ew_cols      : int
}

type zoom_t = Hour | HalfHour | QuarterHour
type side_t = Left | Right


(* keep track of information needed for a line of
 * the timed reminders window *)
type timed_lineinfo_t = {
   tl_filename : string;
   tl_linenum  : string;
   tl_timestr  : string;
   tl_msg      : string;
   tl_start    : float
}


(* keep track of information needed for a line of
 * the untimed reminders window *)
type untimed_lineinfo_t = {
   ul_filename : string;
   ul_linenum  : string;
   ul_msg      : string
}


(* everything you need to know about the interface state goes in this variable *)
type interface_state_t = {
   version           : string;              (* program version string *)
   scr               : screen_t;            (* curses screen with two or three subwindows *)
   run_wyrd          : bool;                (* exit when run_wyrd becomes false *)
   top_timestamp     : float;               (* controls what portion of the schedule is viewable *)
   top_untimed       : int;                 (* controls what portion of untimed reminders are viewable *)
   top_desc          : int;                 (* controls what portion of the reminder descriptions are viewable *)
   selected_side     : side_t;              (* controls which window has the focus *)
   left_selection    : int;                 (* controls which element of the left window is selected *)
   right_selection   : int;                 (* controls which element of the right window is selected *)
   zoom_level        : zoom_t;              (* controls the resolution of the timed window *)
   timed_lineinfo    : timed_lineinfo_t list array;     (* information about the lines of the timed reminder window *)
   untimed_lineinfo  : untimed_lineinfo_t option array; (* same as above, for untimed window *)
   len_untimed       : int;                 (* number of entries in the untimed reminders list *)
   search_regex      : Str.regexp;          (* most recent search string *)
   search_input      : string;              (* buffer to hold search string input *)
   goto_input        : string;              (* buffer to hold 'go to date' input *)
   quick_input       : string;              (* buffer to hold 'quick event' input *)
   is_entering_search: bool;                (* whether or not the user is entering a search string *)
   is_entering_goto  : bool;                (* whether or not the user is entering a date to navigate to *)
   is_entering_quick : bool;                (* whether or not the user is entering a "quick event" *)
   last_timed_refresh: float;               (* the last time the timed window had a complete refresh *)
   rem_buffer        : string               (* buffer that acts as a clipboard for REM strings *)
}
   

(* round to the nearest displayed time value *)
let round_time zoom t =
   match zoom with
   |Hour -> {
       t with Unix.tm_sec = 0;
              Unix.tm_min = 0
    }
   |HalfHour -> {
       t with Unix.tm_sec = 0;
              Unix.tm_min = if t.Unix.tm_min >= 30 then 30 else 0
    }
   |QuarterHour -> {
       t with Unix.tm_sec = 0;
              Unix.tm_min = if t.Unix.tm_min >= 45 then 45
                            else if t.Unix.tm_min >= 30 then 30
                            else if t.Unix.tm_min >= 15 then 15
                            else 0
    }



(* create and initialize an interface with default settings *)
let make (std : screen_t) =
   let curr_time = Unix.localtime ((Unix.time ()) -. 60. *. 60.) in
   let (rounded_time, _) = Unix.mktime (round_time Hour curr_time) in {
      version            = Version.version;
      scr                = std;
      run_wyrd           = true;
      top_timestamp      = if !Rcfile.center_cursor then 
                              rounded_time -. 60.0 *. 60.0 *. (float_of_int ((std.tw_lines / 2) - 2))
                           else
                              rounded_time -. 60.0 *. 60.0;
      top_untimed        = 0;
      top_desc           = 0;
      selected_side      = Left;
      left_selection     = if !Rcfile.center_cursor then (std.tw_lines / 2) - 1 else 2;
      right_selection    = 1;
      zoom_level         = Hour;
      timed_lineinfo     = Array.make std.tw_lines [];
      untimed_lineinfo   = Array.make std.uw_lines None;
      len_untimed        = 0;
      search_regex       = Str.regexp "";
      search_input       = "";
      goto_input         = "";
      quick_input        = "";
      is_entering_search = false;
      is_entering_goto   = false;
      is_entering_quick  = false;
      last_timed_refresh = 0.0;
      rem_buffer         = ""
   }
                                               

(* time increment in float seconds *)
let time_inc (iface : interface_state_t) =
   match iface.zoom_level with
   | Hour        -> 60.0 *. 60.0
   | HalfHour    -> 30.0 *. 60.0
   | QuarterHour -> 15.0 *. 60.0


(* time increment in int minutes *)
let time_inc_min (iface : interface_state_t) =
   match iface.zoom_level with
   | Hour        -> 60
   | HalfHour    -> 30
   | QuarterHour -> 15



let timestamp_of_line iface line =
   iface.top_timestamp +. ((float_of_int line) *. (time_inc iface))




(* arch-tag: DO_NOT_CHANGE_2e912989-cdb2-498a-9bb3-b6d76e94f3a5 *)
