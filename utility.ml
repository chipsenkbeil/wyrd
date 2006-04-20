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

(* utility.ml
 *
 * miscellaneous helper functions that don't really fit elsewhere *)

(* for some reason this is unrecognized if I leave it in txtin_parser.mly *)
exception Txtin_error of string

exception String_of_tm_mon_failure of string
exception String_of_tm_wday_failure of string

(* append a file to a directory, with the proper number
 * of slashes *)
let join_path dirname filename =
   let dir_last   = dirname.[String.length dirname - 1]
   and file_first = filename.[0] in
   if dir_last = '/' && file_first = '/' then
      dirname ^ (Str.string_after filename 1)
   else if dir_last <> '/' && file_first <> '/' then
      dirname ^ "/" ^ filename
   else
      dirname ^ filename


(* If the filename starts with "~", substitute $HOME *)
let expand_file filename =
   if Str.string_before filename 2 = "~/" then
      let homedir = Sys.getenv "HOME" in
      homedir ^ Str.string_after filename 1
   else
      filename


(* Do whatever is necessary to open up a file for writing.  If it already exists,
 * open it as-is.  If it does not exist, make sure that all prefix directories
 * do exist, then open a new file. *)
let open_or_create_out_gen is_binary filename =
   let exp_file = expand_file filename in
   (* Test whether the file exists *)
   if Sys.file_exists exp_file then
      if is_binary then
         open_out_bin exp_file
      else
         open_out exp_file
   else
      (* Check whether all directories exist *)
      let dir_path = Filename.dirname exp_file in
      let dir_list = Str.split (Str.regexp "/+") dir_path in
      (* if necessary, add the first "/" to the first directory *)
      let slash_dir_list =
         if not (Filename.is_relative dir_path) then
            ("/" ^ (List.hd dir_list)) :: (List.tl dir_list)
         else
            dir_list
      in
      let rec make_directories d_list =
         match d_list with
         | [] ->
            ()
         | d :: tail ->
            begin
               try Sys.chdir d
               with Sys_error err_msg ->
                  begin 
                     let _ = Sys.command ("mkdir " ^ d) in
                     Sys.chdir d
                  end
            end;
            make_directories tail
      in
      make_directories slash_dir_list;
      if is_binary then
         open_out_bin (Filename.basename exp_file)
      else
         open_out (Filename.basename exp_file)


let open_or_create_out_bin filename =
   open_or_create_out_gen true filename

let open_or_create_out_ascii filename =
   open_or_create_out_gen false filename



(* open a filename, with tilde expansion *)
let expand_open_in_gen is_binary filename =
   (* If the filename starts with "~", substitute $HOME *)
   if is_binary then
      open_in_bin (expand_file filename)
   else
      open_in (expand_file filename)


let expand_open_in_bin filename =
   expand_open_in_gen true filename

let expand_open_in_ascii filename =
   expand_open_in_gen false filename



let string_of_tm_mon i =
   match i with
   | 0 -> "Jan"
   | 1 -> "Feb"
   | 2 -> "Mar"
   | 3 -> "Apr"
   | 4 -> "May"
   | 5 -> "Jun"
   | 6 -> "Jul"
   | 7 -> "Aug"
   | 8 -> "Sep"
   | 9 -> "Oct"
   |10 -> "Nov"
   |11 -> "Dec"
   | x -> raise (String_of_tm_mon_failure ("unknown month " ^ (string_of_int x)))

let full_string_of_tm_mon i =
   match i with
   | 0 -> "January"
   | 1 -> "February"
   | 2 -> "March"
   | 3 -> "April"
   | 4 -> "May"
   | 5 -> "June"
   | 6 -> "Jul"
   | 7 -> "August"
   | 8 -> "September"
   | 9 -> "October"
   |10 -> "November"
   |11 -> "December"
   | x -> raise (String_of_tm_mon_failure ("unknown month " ^ (string_of_int x)))

let short_string_of_tm_wday i =
   match i with
   | 0 -> "Su"
   | 1 -> "Mo"
   | 2 -> "Tu"
   | 3 -> "We"
   | 4 -> "Th"
   | 5 -> "Fr"
   | 6 -> "Sa"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))

let string_of_tm_wday i =
   match i with
   | 0 -> "Sun"
   | 1 -> "Mon"
   | 2 -> "Tue"
   | 3 -> "Wed"
   | 4 -> "Thu"
   | 5 -> "Fri"
   | 6 -> "Sat"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))

let full_string_of_tm_wday i =
   match i with
   | 0 -> "Sunday"
   | 1 -> "Monday"
   | 2 -> "Tuesday"
   | 3 -> "Wednesday"
   | 4 -> "Thursday"
   | 5 -> "Friday"
   | 6 -> "Saturday"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))



(* it's useful to have an empty date record to save some typing *)
let empty_tm = {
   Unix.tm_sec   = 0;
   Unix.tm_min   = 0;
   Unix.tm_hour  = 0;
   Unix.tm_mday  = 1;
   Unix.tm_mon   = 0;
   Unix.tm_year  = 1900;
   Unix.tm_wday  = 0;
   Unix.tm_yday  = 0;
   Unix.tm_isdst = false
}





(* arch-tag: DO_NOT_CHANGE_a87790db-2dd0-496c-9620-ed968f3253fd *)
