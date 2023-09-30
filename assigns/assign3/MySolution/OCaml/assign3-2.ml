(* Assignment 3-2 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec
list_subsets
(xs: 'a list): 'a list list =
  let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs)
in

list_foldleft(xs)([[]])(fun(r0)(x0) -> 
  let newset = list_map(r0)(fun subset -> (x0 :: subset)) 
  in
  list_append(r0)(newset)
  )
;; 