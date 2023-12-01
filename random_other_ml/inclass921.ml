
#use "./../classlib/OCaml/MyOCaml.ml";;

type 'a mylist = 
  MyNil | MyCons of 'a * 'a mylist

;;

let rec
mylist_foreach
(xs: 'a mylist)(work: 'a -> unit): unit = 
match xs with
  | MyNil -> ()
  | MyCons(x1, xs) -> (work x1); mylist_foreach(xs)(work)
;;

let rec
mylist_foldleft
(xs: 'a mylist)
(r0: 'r0) (fopr: 'r0 -> 'a -> 'r0): 'r0 = 
(* fun xs res fopr -> foreach_to_foldleft(mylist_foreach)(xs) (res)(fopr) *)

match xs with 
|MyNil -> r0
|MyCons(x1, xs) -> mylist_foldleft(xs)(fopr(r0)(x1))(fopr)
;;