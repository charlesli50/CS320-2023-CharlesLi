(* Assignment 0-3 -- cli50@bu.edu -- for CS320 *)

#use "../assign0.ml";;

let rec int2str(i0: int): string = 

  let rec digits x:int = if x < 10 then 1 else 1 + digits (x / 10) in

  let rec 
  getChar(x: int) (i: int) : char = 
  (* print_int i; *)
  let x = abs x in
  if i <= 0 then 
    let c = x mod 10 in
    chr(c + 48)
    else  
    getChar (x/10) (i - 1)
  in  

  let len = if i0 >= 0 then digits i0 else digits (-1 * i0) in 


  if i0 >= 0 then
  string_init len (fun i -> getChar i0 (len-i - 1)) (* for each digit, get char of index i from i0 *)
  else 
  string_init (len + 1) (fun i -> if i = 0 then '-' else getChar i0 (len-i))
  

;;

int2str(132);;
int2str(320);;
int2str(1008392);;
int2str(0);;
(* int2str(-132);;
int2str(-320);;
int2str(-1008392);; *)