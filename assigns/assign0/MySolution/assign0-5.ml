(* Assignment 0-5 -- cli50@bu.edu -- for CS320 *)

#use "../assign0.ml";;

let rec stringrev(cs: string): string =
  let rec getChar(i: int): char = 
    string_get(cs, i)
  in

  let len = string_length cs in
  string_init len (fun i -> getChar(len - i - 1))

;;

stringrev "Hello World";;