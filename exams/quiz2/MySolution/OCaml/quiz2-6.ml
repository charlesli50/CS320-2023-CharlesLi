(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_reverse(xs: 'a list): 'a list = 
  list_foldright(xs)([])(fun x acc -> 
      list_append(acc)([x])
    )
  ;;
