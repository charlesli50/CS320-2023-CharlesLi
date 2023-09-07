(* Assignment 0-5 -- cli50@bu.edu -- for CS320 *)

let rec stringrev(cs: string): string =
  stringrev((String.sub cs 1 (String.length cs))) ^ String.sub cs 0 1

;;

stringrev("Hello World");;