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
   assert (keypad std true);
   assert (cbreak ());
   assert (noecho ());
   Interface_main.create_windows std;;

let iface = Interface.make (initialize_screen ());;


try
   Interface_main.run iface
with error ->
   endwin ();
   Printf.fprintf stderr "Caught error at toplevel:\n%s\n" (Printexc.to_string error);;


(* For some reason this call fails if it is moved to interface_draw... *)
endwin ();;



(* arch-tag: DO_NOT_CHANGE_eeac13df-e93f-4359-8b70-44fefc40e225 *)
