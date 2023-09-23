(* Assignment 2-1 -- cli50@bu.edu -- for CS320 *)
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



let xs0 = MyNil
let xs1 = MyCons(1, xs0)
let xs2 = MySnoc(xs0, 1)
let xs3 = MyAppend2(xs1, xs2)
let xs4 = MyReverse(xs3)
let xs5 = MyAppend2(xs4, xs4)
let xs6 = MyAppend2(xs5, xs5)
let xs7 = MyAppend2(xs6, xs6)
;;

let s0 = mylist_length xs0;;
let s1 = mylist_length xs1;;
let s2 = mylist_length xs2;;
let s3 = mylist_length xs3;;
let s4 = mylist_length xs4;;
let s5 = mylist_length xs5;;
let s6 = mylist_length xs6;;
let s7 = mylist_length xs7;;