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

(* interface.ml
 * This file defines the data structure (a record) that stores the
 * interface state. *)


open Curses;;

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
   mw_cols      : int
}

(* everything you need to know about the interface state goes in this variable *)
type interface_state_t = {
   version                     : string;                        (* program version string *)
   scr                         : screen_t;                      (* curses screen with two or three subwindows *)
   run_remic                   : bool;                          (* exit when run_remic becomes false *)
   top_timestamp               : Unix.tm                        (* controls what portion of the schedule is viewable *)
}
   

(* create and initialize an interface with default settings *)
let make (std : screen_t) =
   let curr_time = Unix.localtime (Unix.time ()) in
   let rounded_time = {
      curr_time with Unix.tm_sec = 0;
                     Unix.tm_min = 0
   } in {
      version       = Version.version;
      scr           = std;
      run_remic     = true;
      top_timestamp = rounded_time
   }
                                               



(* arch-tag: DO_NOT_CHANGE_2e912989-cdb2-498a-9bb3-b6d76e94f3a5 *)
