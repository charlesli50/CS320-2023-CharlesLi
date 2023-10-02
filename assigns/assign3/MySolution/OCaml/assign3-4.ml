(* Assignment 3-4 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let
list_of_buddies(word: string): string list = 
let replace_char (word: string) (index: int) (c0: char): string = 
  string_make_fwork( fun work -> 
    let strlen = string_length word in
    int1_foreach(strlen)(fun i -> 
      if(i = index) then work(c0) else work(string_get_at(word)(i))
      )
  )
in
let for_all_letters(word: string)(index: int): string list = 
  list_make_fwork(fun work ->
    int1_foreach(26)(fun i -> 
        let rep_char = chr(i + ord 'a') in
        if(rep_char != string_get_at(word)(index)) then
          work(replace_char(word)(index)(rep_char))
      )
    )
  in
let strlen = string_length word in
let res = list_make_fwork(fun work ->
  int1_foreach(strlen)(fun i ->
    work(for_all_letters(word)(i))
  )
)in
list_concat(res)
;;


let s = list_of_buddies("hello");;