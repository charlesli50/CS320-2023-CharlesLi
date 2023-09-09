(* Assignment 0-4 -- cli50@bu.edu -- for CS320 *)

let str2int(x: string): int = 
  let rec
  getInts (x: string) (i: int): int =
    if String.length x > i then
      let digit = String.get x i
      in
      let rec getExponent (p: int): int = 
        if p > 1 then
        10 * getExponent(p - 1) 
        else 1
      in
      (Char.code digit - 48) * getExponent(String.length x - i) + (getInts x (i+1))
    else
    0
in 
getInts x 0
;;
