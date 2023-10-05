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

let list_append(xs: 'a list)(ys: 'a list): 'a list =
let new_xs = list_reverse(xs) in
let reversed = 

list_foldleft(ys)(new_xs)(fun acc x -> 
   (* acc :: x *)
   match acc with 
   | [] ->  ys
   | acc -> x :: acc
  ) in

list_reverse (reversed)

;;