(*  Wyrd -- a curses-based front-end for Remind
 *  Copyright (C) 2005, 2006, 2007, 2008, 2010 Paul Pelzl
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
 *  Bug reports can be entered at http://bugs.launchpad.net/wyrd .
 *  For anything else, feel free to contact Paul Pelzl at <pelzlpj@gmail.com>.
 *)

(* OCaml binding for setlocale(), required to kick ncurses into
 * properly rendering non-ASCII chars. *)

type t = LC_ALL | LC_COLLATE | LC_CTYPE | LC_MONETARY |
         LC_NUMERIC | LC_TIME | LC_MESSAGES |
         LC_UNDEFINED of int

external setlocale_int : int -> string -> string = "ml_setlocale"

val setlocale : t -> string -> string


