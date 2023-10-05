(* ************************************************ *)

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

exception Empty
let list_last(xs: 'a list): 'a = 
match xs with 
| [] -> raise Empty
| xs -> list_foldright(xs)(-1)(fun x0 acc -> 
    if acc = -1 then x0 else acc
  )
;;

let s = list_last([5;2;3;5;4;5]);;
let st = list_last([5;4;3;2;1]);;
