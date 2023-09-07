(* Assignment 0-3 -- cli50@bu.edu -- for CS320 *)

let rec int2str(x: int): string = 

if x >= 10 then 

""^int2str(x/10)^String.make 1 (Char.chr (48 + (x mod 10))) 
else String.make 1 (Char.chr (48+x))
;;

(* int2str(132);;
int2str(320);;
int2str(1008746392);;
int2str(0);;
int2str(-1);; *)