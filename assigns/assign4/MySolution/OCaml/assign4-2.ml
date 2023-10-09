(* Assignment 4-2 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let theNatPairs: (int * int) stream = fun () -> 
  let rec streamize(left: int)  (right: int)  (start: int): (int * int) stream = fun () ->
    if left = 0 then
      let new_start = start + 1 in 
      StrCons(((new_start), 0), streamize(new_start) (0) (new_start))
    else 
      let new_left = left-1 in
      let new_right = right + 1 in 
      StrCons((new_left, new_right), streamize(new_left)(new_right)(start))

  in

StrCons( (0, 0), streamize(0)(0)(0) )
;;