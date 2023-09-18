(* Assignment 1-2 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

  (* else if i1 = len1 then 
    (work (string_get_at cs i2); string_merge_rec (i1 + 1) (i2)) *)
  (* else if i2 = len2 then  *)
    (* (work (string_get_at cs i1); string_merge_rec (i1) (i2 + 1)) *)

let
string_merge (cs1:string) (cs2:string): string =
  let len1 = string_length (cs1) in
  let len2 = string_length (cs2) in
  let rec string_merge_rec(i1: int) (i2: int) (work) : unit = 
  (* let rec string_merge_rec(i1: int) (i2: int) : unit =  *)
    if i1 < len1 && i2 < len2 then 
      if string_get_at(cs1) (i1) < string_get_at(cs2) (i2) then 
        (work(string_get_at(cs1) (i1)); string_merge_rec(i1 + 1)(i2) work)
      else
        (work(string_get_at(cs2) (i2)); string_merge_rec(i1)(i2 + 1) work)
    else if i2 = len2 then 
    (int1_foreach(len1 -i1)(fun i -> work(string_get_at(cs1)(i1 +i)))) 
    else if i1 = len1 then 
    (int1_foreach(len2 -i2)(fun i -> work(string_get_at(cs2)(i2 +i)))) 

  (* both have chars, one have chars, other has chars *)

in 
string_make_fwork(string_merge_rec(0)(0));;
;;
let s1 = string_merge("")("abcde");;
let s2 = string_merge("135")("246");;
let s3 = string_merge("EDCBA")("");;