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

exception String_of_tm_mon_failure of string

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


(* Draw the one-line help window at the top of the screen *)
let draw_help (iface : interface_state_t) =
   wattron iface.scr.help_win ((WA.color_pair 1) lor WA.bold lor WA.underline);
   let s = "t:new timed    u:new untimed    <enter>:edit    z:zoom" in
   let blanks = String.make (iface.scr.hw_cols - (String.length s) - 1) ' ' in
   assert (mvwaddstr iface.scr.help_win 0 0 (s ^ blanks));
   assert (wnoutrefresh iface.scr.help_win)


let time_inc iface =
   match iface.zoom_level with
   | Hour        -> 60
   | HalfHour    -> 30
   | QuarterHour -> 15


(* Draw the vertical date strip at the left of the timed window.
 * Note: non-trivial. *)
let draw_date_strip (iface : interface_state_t) =
   Printf.fprintf stderr "draw_date_strip ()\n";
   flush stderr;
   (* determine the line numbers of any date changes within the
    * timed window *)
   let rec check_timestamp date_changes timestamp line =
      if line >= iface.scr.tw_lines then
         date_changes
      else
         let temp = {
            timestamp with 
              Unix.tm_min = timestamp.Unix.tm_min + (time_inc iface)
         } in
         let (_, next_timestamp) = Unix.mktime temp in
         if timestamp.Unix.tm_min = 0 && timestamp.Unix.tm_hour = 0 then
            check_timestamp ((line, timestamp) :: date_changes) next_timestamp (succ line)
         else
            check_timestamp date_changes next_timestamp (succ line)
   in
   let date_changes = List.rev (check_timestamp [] iface.top_timestamp 0) in
   let rec print_date_lines c =
      match c with
      | [] -> ()
      | (line, _) :: tail ->
           Printf.fprintf stderr "line %2d\n" line;
           print_date_lines tail
   in
   print_date_lines date_changes;
   flush stderr;
   let date_chars = 
      (* generate a string to represent the vertical strip *)
      if List.length date_changes > 0 then begin
         (* special case for the top date string *)
         let (line, timestamp) = List.hd date_changes in
         let top_date_str = 
            let temp = {
               timestamp with Unix.tm_mday = pred timestamp.Unix.tm_mday
            } in
            let (_, prev_day) = Unix.mktime temp in
            if line >= 7 then
               (Printf.sprintf " %s %.2d" (string_of_tm_mon prev_day.Unix.tm_mon) 
                   prev_day.Unix.tm_mday) ^ (String.make (line - 7) ' ')
            else
               Str.last_chars
               (Printf.sprintf " %s %.2d" (string_of_tm_mon prev_day.Unix.tm_mon) 
                   prev_day.Unix.tm_mday) line
         in
         (* all other dates are just drawn at the top of their respective windows *)
         let rec add_date date_str changes =
            match changes with
            | [] -> date_str
            | (line, timestamp) :: tail -> 
               let s_len = 
                  if List.length tail > 0 then
                     let (next_line, _) = List.hd tail in
                     next_line - line
                  else
                     iface.scr.tw_lines - line
               in
               let temp_s = 
                  (Printf.sprintf "-%s %.2d" (string_of_tm_mon timestamp.Unix.tm_mon) 
                      timestamp.Unix.tm_mday) ^ (String.make 100 ' ')
               in
               add_date (date_str ^ (Str.string_before temp_s s_len)) tail
         in
         add_date top_date_str date_changes
      end else
         (Printf.sprintf "%s %.2d" (string_of_tm_mon iface.top_timestamp.Unix.tm_mon) 
             iface.top_timestamp.Unix.tm_mday) ^ (String.make (iface.scr.tw_lines - 6) ' ') 
   in
   let acs = get_acs_codes () in
   for i = 0 to pred iface.scr.tw_lines do
      if date_chars.[i] = '-' then begin
         wattron iface.scr.timed_win ((WA.color_pair 5) lor WA.bold);
         assert (mvwaddch iface.scr.timed_win i 0 acs.Acs.hline)
      end else begin
         wattron iface.scr.timed_win ((WA.color_pair 4) lor WA.bold);
         assert (mvwaddch iface.scr.timed_win i 0 (int_of_char date_chars.[i]))
      end
   done;
   (* draw the remaining vertical strip *)
   wattron iface.scr.timed_win ((WA.color_pair 5) lor WA.bold);
   mvwvline iface.scr.timed_win 0 1 acs.Acs.vline iface.scr.tw_lines;
   assert (wnoutrefresh iface.scr.timed_win);
   assert (doupdate ())








(* arch-tag: DO_NOT_CHANGE_9ff0fd0c-6eb1-410f-8fcf-6dfcf94b346a *)
