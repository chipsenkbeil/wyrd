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

open Interface;;
open Curses;;


let initialize_screen () =
   let std = initscr () in
   assert (start_color ());
   assert (init_pair 1 Color.green Color.blue);
   assert (init_pair 2 Color.cyan Color.black);
   assert (init_pair 3 Color.yellow Color.blue);
   assert (init_pair 4 Color.cyan Color.black);
   assert (init_pair 5 Color.cyan Color.blue);
   assert (init_pair 6 Color.yellow Color.red);
   assert (init_pair 7 Color.blue Color.black);
   assert (init_pair 8 Color.white Color.blue);
   assert (init_pair 9 Color.magenta Color.black);
   assert (keypad std true);
   assert (cbreak ());
   assert (noecho ());
   assert (curs_set 0);
   Interface_main.create_windows std;;

let iface = Interface.make (initialize_screen ());;


try
   Interface_main.run iface
with error ->
   endwin ();
   Printf.fprintf stderr "Caught error at toplevel:\n%s\n" (Printexc.to_string error);;


(* arch-tag: DO_NOT_CHANGE_eeac13df-e93f-4359-8b70-44fefc40e225 *)
