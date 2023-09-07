(* Assignment 0-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

let rec 
fact(x: int): int = 
if x > 0 then x * fact(x-1) else 1
;;

(* ****** ****** *)

let fact10 = fact(10);;

(* first factorial that causes the function to return 0 *)

let rec
myloop(x:int): int = 
if fact(x) = 0 then x else myloop(x+1)
;;


(* ****** ****** *)

myloop(1);;


(* end of [CS320-2023-Fall-assign0-1.ml] *)