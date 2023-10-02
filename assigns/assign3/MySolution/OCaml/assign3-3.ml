(* Assignment 3-3 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let
list_subsets
(xs: 'a list): 'a list list =
let input_list = list_reverse(xs)
in
  let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs)
in

list_foldleft(input_list)([[]])(fun(r0)(x0) -> 
  let newset = list_map(r0)(fun subset -> (x0 :: subset)) 
  in
  list_append(r0)(newset)
  )
;; 

let list_nchoose
(xs: 'a list)(n0: int): 'a list list =
let subsets = list_subsets(xs)
in 
  let check_if_nlength (xs: 'a list): bool = 
    let length = list_foldleft(xs)(0)(fun r0 x0 ->
      r0 + 1)
    in
  n0 = length
in

list_foldleft(subsets)([])(fun (r0)(x0) -> 
  if (check_if_nlength(x0)) then list_append(r0)([x0]) else r0
)

(* filter test work *)
(* list_make_filter(check_if_nlength)(fun work -> list_foreach(subsets)(fun curVal -> work(curVal))) *)
;;

let sub = list_subsets([1;2;3;4;5]);;
(* let n = check_if_nlength([1;2;3;4]);; *)