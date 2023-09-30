(* Assignment 3-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec 
matrix_transpose(xss: 'a list list): 'a list list = 
(* zip the matrices into lists, then add together *)
  let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs)
in

list_foldleft(xs)([[]])(fun(r0)(x0) -> 
  let newset = list_map(r0)(fun subset -> (x0 :: subset)) 
  in
  list_append(r0)(newset)
  )
;;

(* 
 let rec matrix_transpose xss =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ -> List.map List.hd xss :: matrix_transpose (List.map List.tl xss) *)
  