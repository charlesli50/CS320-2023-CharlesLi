(* Assignment 0-2 -- cli50@bu.edu -- for CS320 *)


(* returns whether input is prime, test for 0, 1 *)
let isPrime(x: int): bool = 
(* if x > 1 then testPrime x (2) else false *)

  let rec
  testPrime(x:int) (y:int): bool = 
  if(x = y) then
    true else 
    if x mod y = 0 then false
    else testPrime x (y+1)

in 
if x > 1 then testPrime x (2) else false
;;

(* 
isPrime(0);;
isPrime(1);;
isPrime(5);;
isPrime(9);;
isPrime(97);; *)
