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

(* Draw the one-line help window at the top of the screen *)
let draw_help (iface : interface_state_t) =
   wattron iface.scr.help_win ((WA.color_pair 1) lor WA.bold lor WA.underline);
   let s = "t:new timed    u:new untimed    <enter>:edit    z:zoom" in
   let blanks = String.make (iface.scr.hw_cols - (String.length s) - 1) ' ' in
   assert (mvwaddstr iface.scr.help_win 0 0 (s ^ blanks));
   assert (wnoutrefresh iface.scr.help_win)








(* arch-tag: DO_NOT_CHANGE_9ff0fd0c-6eb1-410f-8fcf-6dfcf94b346a *)
