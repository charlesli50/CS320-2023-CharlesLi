(* Assignment 1-3 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* check if all increasing, all decreasing or more than 2 numbers *)

let string_avoid_132(cs:string): bool =
  (* if starts out decreasing, then  *)
  (* if it becomes smaller than the one compared to, pass that in *)
  (* int1_foreach(n0:int) (string_get_at(cs) (n0)) *)

  let rec is_string_increasing (cs:string) (i0:int): bool =
    let len = string_length cs in
    if i0 < (len - 1) then
      let current_char = string_get_at (cs) (i0) in
      let next_char = string_get_at (cs) (i0+1)  in
      if current_char < next_char then
        is_string_increasing cs (i0 + 1)
      else
        false
    else
      true 
  in
  let rec is_string_decreasing (cs:string) (i0:int): bool =
    let len = string_length cs in
    if i0 < (len - 1) then
      let current_char = string_get_at (cs) (i0) in
      let next_char = string_get_at (cs) (i0+1)  in
      if current_char > next_char then
        is_string_decreasing cs (i0 + 1)
      else
        false
    else
      true
      
in
(is_string_increasing(cs) 0) || (is_string_decreasing(cs) 0) && (string_length cs > 2)
(* is_string_decreasing(cs) 0 *)


;;

string_avoid_132("123456789");;
string_avoid_132("987654321");;
string_avoid_132("12345654789");;
string_avoid_132("132");;
string_avoid_132("123");;