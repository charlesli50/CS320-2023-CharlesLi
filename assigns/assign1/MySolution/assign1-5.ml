(* Assignment 1-5 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;


let string_longest_ascend (xs:string): string =
  (* let prevChar = string_get_at(0) in *)

  let ascend (c:char): bool = 
  true in 
      (* if char is greater than previous character, true, else false *)
string_make_fwork (fun(work) -> 
  string_foreach cs 
  (fun char -> if (
    ascend(char) then work(char) )
;;

