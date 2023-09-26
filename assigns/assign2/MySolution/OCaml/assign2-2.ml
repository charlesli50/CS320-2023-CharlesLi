(* Assignment 2-2 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;


let rec
mylist_length
(xs: 'a mylist): int =
  let rec mylist_length_rec (xs: 'a mylist) (len: int): int = 
    match xs with
    | MyNil -> len
    | MyCons(x1, xs) -> mylist_length_rec (xs) (len + 1)
    | MySnoc(xs, x1) -> mylist_length_rec (xs) (len + 1)
    | MyReverse(xs) -> mylist_length_rec (xs) (len)
    | MyAppend2(xs1, xs2) -> mylist_length_rec(xs1) (len) + mylist_length_rec(xs2) (len)

  in
mylist_length_rec xs 0
;;


let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = 
  (* let len = mylist_length xs in *)
  match xs with
  | MyNil -> mylist_subscript_exn()
  | MyCons(x1, xs) -> if i0 <= 0 then x1 else mylist_get_at(xs) ( i0-1 )
  | MySnoc(xs, x1) -> if i0 = (mylist_length xs) then x1 else mylist_get_at(xs) ( i0 )
  | MyReverse(xs) -> if i0 < mylist_length xs then mylist_get_at(xs) ( (mylist_length xs) - 1 - i0) else mylist_subscript_exn()
  | MyAppend2(xs1, xs2) -> if i0 >= mylist_length(xs1) 
    then mylist_get_at(xs2)(i0 - mylist_length(xs1)) 
    else mylist_get_at(xs1)(i0)
;;

let xs0 = MyNil
let xs1 = MyCons(10, xs0)
let xs2 = MySnoc(xs0, -10)
let xs3 = MyAppend2(xs1, xs2)
let xs4 = MyReverse(xs3)
let xs5 = MyAppend2(xs4, xs4)
let xs6 = MyAppend2(xs5, xs5)
let xs7 = MyAppend2(xs6, xs6)
;;

(* let test = MyCons(1, MyCons(2, MyNil))
let s1 = mylist_get_at test (0);; *)

let s0 = mylist_get_at xs0 (0);;
let s1 = mylist_get_at xs1 (0);;
let s2 = mylist_get_at xs2 (0);;
let s3 = mylist_get_at xs3 (0);;
let s4 = mylist_get_at xs4 (0);;
let s5 = mylist_get_at xs5 (0);;
let s6 = mylist_get_at xs6 (0);;
(* let s7 = mylist_get_at xs7 (0);; *)
(* let s0 = mylist_get_at xs0 (0);; *)