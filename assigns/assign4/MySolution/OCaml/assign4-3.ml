(* Assignment 4-3 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* ****** ****** *)

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

(* ****** ****** *)


let rec 
gtree_streamize_dfs(xs: 'a gtree): 'a stream = 
match xs with
| GTnil -> StrCons(_, xs)
| x1 :: xs -> StrCons(x1, xs);;