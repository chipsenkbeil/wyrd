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

(* rcfile.ml
 * This file includes everything associated with processing the wyrdrc file.
 * In particular, this includes a number of hashtables used to store the
 * bindings of curses keypresses to calendar operations.
 * Adapted from rcfile code in Orpie, a curses RPN calculator. *)

open Genlex
open Curses


exception Config_failure of string
let config_failwith s = raise (Config_failure s)

type operation_t = | ScrollUp | ScrollDown | NextDay | PrevDay 
                   | NextWeek | PrevWeek | Home | Zoom | Edit
                   | NewTimed | NewUntimed | SwitchWindow | FindNext 
                   | Quit


(* These hashtables store conversions between curses keys and the operations
 * they are associated with. *)
let table_key_command = Hashtbl.create 20
let table_command_key = Hashtbl.create 20


(* Default reminders file *)
let reminders_file = ref "~/.reminders"
(* Default editing command strings *)
let edit_old_command = ref "vim +%n + %f"
let edit_new_command = ref "vim -c '$' %f"
(* Default thresholds for calendar colorization *)
let busy_level1 = ref 2
let busy_level2 = ref 4
let busy_level3 = ref 6
let busy_level4 = ref 8
(* List of included rc files *)
let included_rcfiles : (string list) ref = ref []


let command_of_key key =
   Hashtbl.find table_key_command key
let key_of_command command =
   Hashtbl.find table_command_key command


let key_of_operation (op : operation_t) =
   Hashtbl.find table_command_key op


let decode_single_key_string key_string =
   let decode_alias str =
      match str with
      |"<esc>"       -> 27
      |"<tab>"       -> 9
      |"<enter>"     -> Key.enter
      |"<return>"    -> 10
      |"<insert>"    -> Key.ic
      |"<delete>"    -> Key.dc
      |"<home>"      -> Key.home
      |"<end>"       -> Key.end_
      |"<pageup>"    -> Key.ppage
      |"<pagedown>"  -> Key.npage
      |"<space>"     -> 32
      |"<backspace>" -> Key.backspace
      |"<left>"      -> Key.left
      |"<right>"     -> Key.right
      |"<up>"        -> Key.up
      |"<down>"      -> Key.down
      |"<f1>"        -> (Key.f 1)
      |"<f2>"        -> (Key.f 2)
      |"<f3>"        -> (Key.f 3)
      |"<f4>"        -> (Key.f 4)
      |"<f5>"        -> (Key.f 5)
      |"<f6>"        -> (Key.f 6)
      |"<f7>"        -> (Key.f 7)
      |"<f8>"        -> (Key.f 8)
      |"<f9>"        -> (Key.f 9)
      |"<f10>"       -> (Key.f 10)
      |"<f11>"       -> (Key.f 11)
      |"<f12>"       -> (Key.f 12)
      |_             -> 
         if String.length key_string = 1 then
            int_of_char str.[0]
         else
            config_failwith ("Unrecognized key \"" ^ str ^ "\"")
   in
   (* This regexp is used to extract the ctrl and meta characters from a string
    * representing a keypress.
    * It matches \\M\\C or \\C\\M or \\C or \\M (or no such characters) followed
    * by an arbitrary string. *)
   (* Note: is there a way to use raw strings here?  Getting tired of those
    * backslashes...*)
   let cm_re = Str.regexp
   "^\\(\\(\\\\M\\\\C\\|\\\\C\\\\M\\)\\|\\(\\\\M\\)\\|\\(\\\\C\\)\\)?\\(<.+>\\|.\\)"
   in
   if Str.string_match cm_re key_string 0 then
      let has_meta_ctrl =
         try let _ = Str.matched_group 2 key_string in true
         with Not_found -> false
      and has_meta =
         try let _  = Str.matched_group 3 key_string in true
         with Not_found -> false
      and has_ctrl =
         try let _ = Str.matched_group 4 key_string in true
         with Not_found -> false
      and main_key = Str.matched_group 5 key_string in
      if has_meta_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let mc_chtype = ((int_of_char uc_main_key.[0]) + 64) in
            let mc_str = "M-C-" ^ uc_main_key in
            (mc_chtype, mc_str)
         else
            config_failwith ("Cannot apply \\\\M\\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_meta then
         if String.length main_key = 1 then
            let m_chtype = ((int_of_char main_key.[0]) + 128) in
            let m_str = "M-" ^ main_key in
            (m_chtype, m_str)
         else
            config_failwith ("Cannot apply \\\\M to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let c_chtype = ((int_of_char uc_main_key.[0]) - 64) in
            let c_str = "C-" ^ uc_main_key in
            (c_chtype, c_str)
         else
            config_failwith ("Cannot apply \\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else 
         let octal_regex = Str.regexp "^0o" in
         try
            let pos = Str.search_forward octal_regex key_string 0 in
            ((int_of_string key_string), ("\\" ^ Str.string_after key_string
            2))
         with
            _ -> ((decode_alias main_key), main_key)
   else
      config_failwith ("Unable to match binding string with standard regular expression.")



(* Register a key binding.  This adds hash table entries for translation
 * between curses chtypes and commands (in both directions). *)
let register_binding_internal k k_string op =
   Hashtbl.add table_key_command k op;
   Hashtbl.add table_command_key op k_string


(* convenience routine for previous *)
let register_binding key_string op =
   (* given a string that represents a character, find the associated
    * curses chtype *)
   let k, string_rep = decode_single_key_string key_string in
   register_binding_internal k string_rep op


(* unregister a binding *)
let unregister_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_command k in
      Hashtbl.remove table_key_command k;
      Hashtbl.remove table_command_key op
   with Not_found -> ()


(* Remove a key binding. *)
let remove_binding k op =
   Hashtbl.remove table_key_command k;
   Hashtbl.remove table_command_key op


(* translate a command string to the command type it represents *)
let operation_of_string command_str =
   begin match command_str with
   |"scroll_up"     -> ScrollUp
   |"scroll_down"   -> ScrollDown
   |"next_day"      -> NextDay
   |"previous_day"  -> PrevDay
   |"next_week"     -> NextWeek
   |"previous_week" -> PrevWeek
   |"home"          -> Home
   |"zoom"          -> Zoom
   |"edit"          -> Edit
   |"new_timed"     -> NewTimed
   |"new_untimed"   -> NewUntimed
   |"switch_window" -> SwitchWindow
   |"find_next"     -> FindNext
   |"quit"          -> Quit
   |_               -> config_failwith ("Unknown command name \"" ^ command_str ^ "\"")
   end


(* Parse a line from a configuration file.  This operates on a stream
 * corresponding to a non-empty line from the file.  It will match commands
 * of the form
 *    bind key command
 * where 'key' is either a quoted string containing a key specifier or an octal
 * key representation of the form \xxx (unquoted), and multiple_keys is a quoted
 * string containing a number of keypresses to simulate.
 *)
let parse_line line_stream = 
   match line_stream with parser
   | [< 'Kwd "include" >] ->
      begin match line_stream with parser
      | [< 'String include_file >] ->
         included_rcfiles := include_file :: !included_rcfiles
      | [< >] ->
         config_failwith ("Expected a filename string after \"include\"")
      end
   | [< 'Kwd "bind" >] -> 
      let bind_key key = 
         begin match line_stream with parser
         | [< 'Ident command_str >] ->
            let command = operation_of_string command_str in
            register_binding key command
         | [< >] ->
            config_failwith ("Expected a command name after \"bind \"" ^ key ^ "\"")
         end
      in
      begin match line_stream with parser
      | [< 'String k >] -> 
         bind_key k
      | [< 'Ident "\\" >] ->
         begin match line_stream with parser
         | [< 'Int octal_int >] ->
            begin
               try
                  let octal_digits = "0o" ^ (string_of_int octal_int) in
                  bind_key octal_digits 
               with 
                  (Failure "int_of_string") -> config_failwith "Expected octal digits after \"\\\""
            end
         | [< >]  ->
            config_failwith "Expected octal digits after \"\\\""
         end
      | [< >] ->
         config_failwith "Expected a key string after keyword \"bind\""
      end
   | [< 'Kwd "unbind" >] ->
      begin match line_stream with parser
      | [< 'String k >] ->
         unregister_binding k
      | [< >] ->
         config_failwith ("Expected a key string after keyword \"unbind\"")
      end
   | [< 'Kwd "set" >] ->
      begin match line_stream with parser
      | [< 'Ident "reminders_file" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String f >] ->
               reminders_file := f
            | [< >] ->
               config_failwith ("Expected a filename after " ^
               "\"set reminders_file = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set reminders_file\"")
         end
      | [< 'Ident "edit_old_command" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String c >] ->
               edit_old_command := c
            | [< >] ->
               config_failwith ("Expected a command string after " ^
               "\"set edit_old_command = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set edit_old_command\"")
         end
      | [< 'Ident "edit_new_command" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String c >] ->
               edit_new_command := c
            | [< >] ->
               config_failwith ("Expected a command string after " ^
               "\"set edit_new_command = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set edit_new_command\"")
         end
      | [< 'Ident "busy_level1" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String num >] ->
               begin try
                  busy_level1 := int_of_string num
               with _ ->
                  config_failwith ("Expected an integer string after " ^
                  "\"set busy_level1 = \"")
               end
            | [< >] ->
               config_failwith ("Expected an integer string after " ^
               "\"set busy_level1 = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set busy_level1\"")
         end
      | [< 'Ident "busy_level2" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String num >] ->
               begin try
                  busy_level2 := int_of_string num
               with _ ->
                  config_failwith ("Expected an integer string after " ^
                  "\"set busy_level2 = \"")
               end
            | [< >] ->
               config_failwith ("Expected an integer string after " ^
               "\"set busy_level2 = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set busy_level2\"")
         end
      | [< 'Ident "busy_level3" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String num >] ->
               begin try
                  busy_level3 := int_of_string num
               with _ ->
                  config_failwith ("Expected an integer string after " ^
                  "\"set busy_level3 = \"")
               end
            | [< >] ->
               config_failwith ("Expected an integer string after " ^
               "\"set busy_level3 = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set busy_level3\"")
         end
      | [< 'Ident "busy_level4" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String num >] ->
               begin try
                  busy_level4 := int_of_string num
               with _ ->
                  config_failwith ("Expected an integer string after " ^
                  "\"set busy_level4 = \"")
               end
            | [< >] ->
               config_failwith ("Expected an integer string after " ^
               "\"set busy_level4 = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set busy_level4\"")
         end
      | [< >] ->
         config_failwith ("Unmatched variable name after \"set\"")
      end
   | [< 'Kwd "#" >] ->
      ()
   | [< >] ->
      config_failwith "Expected a keyword at start of line";;


(* try opening the rc file, first looking at $HOME/.wyrdrc, 
 * then looking at $PREFIX/etc/wyrdrc *)
let open_rcfile rcfile_op =
   match rcfile_op with
   |None ->
      let home_rcfile =
         let homedir = Sys.getenv "HOME" in
         homedir ^ "/.wyrdrc"
      in
      let rcfile_fullpath = 
         (* expand out any occurrences of ${prefix} that autoconf
          * decides to insert *)
         let prefix_regex = Str.regexp "\\${prefix}" in
         let expanded_sysconfdir = Str.global_replace prefix_regex 
         Install.prefix Install.sysconfdir in
         Utility.join_path expanded_sysconfdir "wyrdrc"
      in
      begin try (open_in home_rcfile, home_rcfile)
      with Sys_error error_str ->
         begin try (open_in rcfile_fullpath, rcfile_fullpath)
         with Sys_error error_str -> failwith 
            ("Could not open configuration file \"" ^ home_rcfile ^ "\" or \"" ^ 
            rcfile_fullpath ^ "\" .")
         end
      end
   |Some file ->
      try (Utility.expand_open_in_ascii file, file)
      with Sys_error error_str -> config_failwith
      ("Could not open configuration file \"" ^ file ^ "\".")



let rec process_rcfile rcfile_op =
   let line_lexer line = 
      make_lexer 
         ["include"; "bind"; "unbind"; "set"; "#"] 
      (Stream.of_string line)
   in
   let empty_regexp = Str.regexp "^[\t ]*$" in
   let config_stream, rcfile_filename = open_rcfile rcfile_op in
   let line_num = ref 0 in
   try
      while true do
         line_num := succ !line_num;
         let line_string = input_line config_stream in
         (* Printf.fprintf stderr "read line %2d: %s\n" !line_num line_string;
         flush stderr; *)
         if Str.string_match empty_regexp line_string 0 then
            (* do nothing on an empty line *)
            ()
         else
            try
               let line_stream = line_lexer line_string in
               parse_line line_stream;
               (* process any included rcfiles as they are encountered *)
               begin match !included_rcfiles with
               |[] -> ()
               |head :: tail -> 
                  included_rcfiles := tail;
                  process_rcfile (Some head)
               end
            with
               |Config_failure s ->
                  (let error_str = Printf.sprintf "Syntax error on line %d of \"%s\": %s"
                  !line_num rcfile_filename s in
                  failwith error_str)
               |Stream.Failure ->
                  failwith (Printf.sprintf "Syntax error on line %d of \"%s\"" 
                  !line_num rcfile_filename)

      done
   with End_of_file ->
      begin
         close_in config_stream;
      end




(* arch-tag: DO_NOT_CHANGE_614115ed-7d1d-4834-bda4-e6cf93ac3fcd *)
