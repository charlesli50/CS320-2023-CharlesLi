(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

exception Empty
let list_last(xs: 'a list): 'a = 
match xs with 
| [] -> raise Empty
| x1 :: xs -> list_foldleft(xs)(x1)(fun acc x -> 
  x)
;;
