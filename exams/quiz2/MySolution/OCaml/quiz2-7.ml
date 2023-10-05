(* ************************************************ *)

(*
Q2-7: 10 points

The following implementation of list_append is not tail-recursive.
Please give an implementation of list_append that is tail-recursive.

Note that you can only use pattern matching and list_foldleft in your
implementation.
 
let rec
list_append(xs: 'a list)(ys: 'a list) =
match xs with
  [] -> ys | x1 :: xs -> x1 :: list_append(xs)(ys)
*)

(* ************************************************ *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;


let rec
list_append(xs: 'a list)(ys: 'a list) =
match xs with
  | [] -> ys 
  | x1 :: xs -> list_foldleft(ys)(xs)(fun acc x -> 
    x1 :: list_append(xs)(ys)
    );;

let s = list_append([1;2;3;4;5]) ([6;7;8;9]);;