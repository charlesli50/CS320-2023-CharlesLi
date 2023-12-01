(* ##################################### *)

(* Continuation-1: *)

let rec f91(x) = if x > 100 then x - 10 else f91(f91(x+11))

(* Please translate f91 into kf91, which is the CPS version of f91. *)

let rec kf91 x k = 
  if x > 100 then k(x - 10)
  else kf91(x+11)(fun res -> kf91(res)(k))


(* ##################################### *)

(* Continuation-2: *)

let rec kfact(x)(k) =

if x > 0 then kfact(x-1)(fun res -> k(x * res)) else k(1)

(* kfact is the CPS version of the usual factorial implementation. *)

(* Say we have *)

let rec kfact2(x)(k) =

if x > 0 then kfact(x-1)(fun res -> k(x * res)) else k(k(1))

(* What is the value if kfact2(3)(fun res -> res)? *)

(* You should not run the code on a computer. Instead, use reasoning. *)
(* 
#####################################

Continuation-related:

How do you implement f91 in a tail-recursive manner

(with no use of continuation)?

##################################### *)

let whatisit1(f)(x) = f(x)(x)

let whatisit2(f)(x)(y) = f(y)(x)

(* What is the type of whatisit1 in OCaml?

What is the most general type of 'whatisit2' in OCaml?

###################################### *)

let rec
f(x) = f(g(x+1))
and
g(x) = g(g(x-2)) + 1

(* Please CPS translate f and g simultaneously. The answer is given as follows: *)

let rec
kf(x)(k) =
kg(x+1)(fun r -> kf r k)
and
kg(x)(k) =
kg(x-2)(fun r -> kg r (fun r -> k(r + 1)))

(* #################################### *)

let rec f(x) = f(2*x)

(* What would happen if you evaluate 'f(1)' in OCaml? *)
(* 
def f(x):

     return f(2*x) *)
(* 
What would happen if you evaluate 'f(1)' in Python?

###################################### *)

let rec f(x) = if x <> 0 then f(2*x) else x

(* If you evaluate f(1) in OCaml, what will happen? Why? *)

(* ###################################### *)

(* Question 1 *)
let fg = fun(f)(g)(x) -> f(g(x))


(* What is the type of fg in OCaml?
1. (’a -> ’a) -> (’b -> ’b) -> (’a -> ’b)
2. (’a -> ’b) -> (’b -> ’c) -> (’a -> ’c)
3. (’a -> ’b) -> (’a -> ’b) -> (’a -> ’b)
4. (’b -> ’c) -> (’a -> ’b) -> (’a -> ’c)
5. None of the above

############################### *)


let
foo = fun x -> x

let

foo = fun x ->
if x > 0 then x * foo(x-1) else 1

let foo1 = foo(1)
let foo2 = foo(2)
(* 
What are the values of foo1 and foo2

##################################### *)

let rec
foo = fun x -> x

let rec

foo = fun x ->
if x > 0 then x * foo(x-1) else 1

let foo1 = foo(1)
let foo2 = foo(2)

(* What are the values of foo1 and foo2

################################### *)

(*
 This question should reinforce the claim that
 references are to be avoided.
*)

let
tricky =
let
global = ref(0) in
let rec
f(i: int) : int =
global := !global + i;                                                                                                                                
if i > 0
then f(i-1) else !global in fun(i:int) -> f(i)
;;                                                                                                                                                    
(*
 What is the value of the expression tricky(10)?
*)

let tricky10 = tricky(10);;         

(* #################################### *)

let
mystream =
let
rec
fstream(n: int): int stream = fun() ->
StrCons(n, stream_map(fstream(n+1))(fun(x) -> x+x+1)) in
fstream(0)
;;                                                                                                                                                    
(*
 The first element of mystream is 0.
 What is the 5th (fifth) element in mystream?
 *)
;;

(* ################################## *)

let rec
fff(n: int): int =
if n = 0 then 0 else 10*fff(n / 2) + n mod 2
;;                                                                                                                                                    
(*
 What is the value of fff(1023)?
*)

(* ################################## *)

type intcont = (int) -> int;;                                                                                                                         

let rec
kf(n: int)(k1: intcont)(k2: intcont): int =
if n = 0
then k1(0) else
kf(n-1)(fun(res) -> k2(res+n))(fun(res) -> k1(res-n))
;;                                                                                                                                                    
let
kf0(n: int): int = kf(n)(fun res -> res)(fun res -> res)
;;                                                                                                                                                    

(*
 What is the value of kf0(10)?
*)

(* ################################ *)