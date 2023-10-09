(* Assignment 4-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* natural series *)

(* ln 2 = 1 - 1/2 + 1/3 - 1/4 + 1/5 
//do the partial sums
*)

let the_ln2_stream: float stream = fun () -> 
(* let the_ln2_stream: float stream = fun () ->  *)
  (* do the lazy eval *)
  (* the params:   number at in series,  current_sum,  sign of the next sum*)
  let rec streamize (denom: float) (sum: float) (sign: float): float stream = fun () ->
    let next_num = sign /. denom in
    let next_sum = sum +. next_num in
    (* _   _   get next sign, +1 or -1 *)
    let next_sign = if (sign = 1.) then -1. else 1. in
    StrCons(next_sum, streamize(denom +. 1.)(next_sum)(next_sign) )
  in

  StrCons(1.,  streamize(2.)(1.)(-1.))
;;
