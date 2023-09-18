(* Assignment 1-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)


#use "./../../../classlib/OCaml/MyOCaml.ml";;

let intrev10(n: int): int = 
  let rec reverse (n:int) (sum: int): int = 
    if n != 0 then 
    let sum = (10*sum) + n mod 10 in
    reverse (n/10) sum
    else sum
  in 
  reverse n 0
;;


intrev10(12345);;