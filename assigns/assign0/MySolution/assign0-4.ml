(* Assignment 0-4 -- cli50@bu.edu -- for CS320 *)

#use "../assign0.ml";;

let str2int(x: string): int = 
  let rec
  getInts (x: string) (i: int): int =
    if string_length x > i then
      let digit = string_get(x, i)
      in
      let rec getExponent (p: int): int = 
        if p > 1 then
        10 * getExponent(p - 1) 
        else 1
      in
      (ord digit - 48) * getExponent(string_length x - i) + (getInts x (i+1))
    else
    0
in 
getInts x 0
;;
