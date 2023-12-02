#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* let interp (s : string) : string list option =  *)


(* <foo, V, C> *)
(* let y = 4 in fun x -> x + y *)
(* <???, y->4 :: e, x+y *)

(* Fun C End;
Call;
Return; *)

(* <f, V, C> goes on the stack? am confused *)

(* [         f :: S | T | V] Fun C End; P
[ <f, V, C> :: S | T | V] P


[ < f, V_f, C> :: a :: S | T | V   ]  Call; P
[ a :: <cc, V, P>   :: S | T | V_f ]  C

continuation passing style? *)


type const = 
| Int of int
| Bool of bool
| Unit
| Sym of string
(* | Closure of string*(string*const)list*(com list) *)
| Closure of const*(const*const)list*(com list)

and com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | IfElse of com list*com list
  | Bind | Lookup
  | Fun of com list | Call | Return
(* and coms = com list *)

let symbol : string parser  =
  let read_sym = 
    satisfy char_islower <|> satisfy char_isdigit
  in
  fun ls ->
  let@ (xs, ls) = many1 read_sym ls in
  Some(list_foldleft(xs)("") (fun acc n -> string_append(acc )(str(n))  ), ls)

let parse_const = 
	(let* _ = char '-' in
	let* x = natural in pure  (Int (-x)))
	<|>
	(let* n = natural in
	pure ( Int n ))
	<|> 
	(let* _ = keyword "True" in 
	pure (Bool (true) ))
	<|> 
	(let* _ = keyword "False" in 
	pure (Bool( false )))
	<|> 
	(let* _ = keyword "Unit" in
	pure (Unit))
  <|>
  (
  let* sym = symbol in 
  pure (Sym ( sym ))
  )

let rec array_len(xs: 'a list): int =
	list_foldleft(xs)(0)(fun acc x ->
		acc + 1)

let rec int2str(i0: int): string = 
	let rec digits x:int = if x < 10 then 1 else 1 + digits (x / 10) in
		let rec getChar(x: int) (i: int) : char = 
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
		
let const_to_string(const): string = 
match const with
| Int n -> int2str n
| Bool true -> "True"
| Bool false -> "False"
| Unit -> "Unit"
| Sym str -> str

let rec parse_com() = 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (fun ls -> 
    let@ (_, ls) = keyword "If" (ls) in 
    let@ (c1, ls) = many (parse_com() << keyword ";" ) (ls) in
    let@ (_, ls) = keyword "Else" (ls) in
    let@ (c2, ls) = many (parse_com() << keyword ";") (ls) in
    let@ (_, ls) = keyword "End" (ls) in 
    pure (IfElse (c1, c2) )(ls))  <|>
  (keyword "Bind" >> pure Bind) <|> 
  (keyword "Lookup" >> pure Lookup) <|>
  (let* _ = keyword "Fun" in
   let* c = many (parse_com() << keyword ";")  in
   let* _ = keyword "End" in
   pure (Fun c)) <|>
  (keyword "Call" >> pure Call) 
  

let parse_prog = many (parse_com() << keyword ";")

let string_parse_c(p: 'a parser)(s: string) =
  p(string_listize(s))

let interp (s: string) : string list option = 
  let rec lookup (x : const) (v : (const * const) list) : const option = 
    match v with
    | (var, value) :: v0 -> if x = var then Some(value) else lookup x v0
    | [] -> None
  in
  let rec evaluate(s : const list) (t : string list) (v : (const * const) list) (p : com list) : string list =
    match p with
    (* termination state returns the trace *)
    | [] -> t
    | Push c :: p0 (* PushStack *) -> evaluate (c :: s) t v p0
    | Swap :: p0 -> 
      (match s with
      | i :: j :: s0   (*swap stack*) -> evaluate ( j :: i :: s0) t v p0
      | _ :: []        (* Die *)      -> evaluate [] ("Panic" :: t) v []
      | []                            -> evaluate [] ("Panic" :: t) v []
      )
    | Pop :: p0 ->
      (match s with
       | _ :: s0 (* PopStack *) -> evaluate s0 t v p0
       | []      (* PopError *) -> evaluate [] ("Panic" :: t) v [])
    | Trace :: p0 ->
      (match s with
       | c :: s0 (* TraceStack *) -> evaluate (Unit  :: s0) (const_to_string c :: t) v p0
       | []      (* TraceError *) -> evaluate [] ("Panic" :: t) v [])
    | Add :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* AddStack *)  -> evaluate (Int (i + j) :: s0) t v p0
       | _ :: _ :: s0         (* AddError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                   (* AddError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []              (* AddError3 *) -> evaluate [] ("Panic" :: t) v [])
    | Sub :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* SubStack *)  -> evaluate (Int (i - j) :: s0) t v p0
       | _ :: _ :: s0         (* SubError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                   (* SubError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []              (* SubError3 *) -> evaluate [] ("Panic" :: t) v [])
    | Mul :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* MulStack *)  -> evaluate (Int (i * j) :: s0) t v p0
       | _ :: _ :: s0         (* MulError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                   (* MulError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []              (* MulError3 *) -> evaluate [] ("Panic" :: t) v [])
    | Div :: p0 ->
      (match s with
       | Int i :: Int 0 :: s0 (* DivError0 *) -> evaluate [] ("Panic" :: t) v []
       | Int i :: Int j :: s0 (* DivStack *)  -> evaluate (Int (i / j) :: s0) t v p0
       | _ :: _ :: s0         (* DivError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                   (* DivError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []              (* DivError3 *) -> evaluate [] ("Panic" :: t) v [])
    | And :: p0 ->
      (match s with
       | Bool a :: Bool b :: s0 (* AndStack *)  -> evaluate (Bool (a && b) :: s0) t v p0
       | _ :: _ :: s0           (* AndError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                     (* AndError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []                (* AndError3 *) -> evaluate [] ("Panic" :: t) v [])
    | Or :: p0 ->
      (match s with
       | Bool a :: Bool b :: s0 (* OrStack *)  -> evaluate (Bool (a || b) :: s0) t v p0
       | _ :: _ :: s0           (* OrError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                     (* OrError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []                (* OrError3 *) -> evaluate [] ("Panic" :: t) v [])
    | Not :: p0 ->
      (match s with
       | Bool a :: s0 (* NotStack  *) -> evaluate (Bool (not a) :: s0) t v p0
       | _ :: s0      (* NotError1 *) -> evaluate [] ("Panic" :: t) v []
       | []           (* NotError2 *) -> evaluate [] ("Panic" :: t) v [])
    | Lt :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* LtStack *)  -> evaluate (Bool (i < j) :: s0) t v p0
       | _ :: _ :: s0         (* LtError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                   (* LtError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []              (* LtError3 *) -> evaluate [] ("Panic" :: t) v [])
    | Gt :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* GtStack *)  -> evaluate (Bool (i > j) :: s0) t v p0
       | _ :: _ :: s0         (* GtError1 *) -> evaluate [] ("Panic" :: t) v []
       | []                   (* GtError2 *) -> evaluate [] ("Panic" :: t) v []
       | _ :: []              (* GtError3 *) -> evaluate [] ("Panic" :: t) v [])
    | IfElse (c1, c2) :: p0 ->       
      (match s with 
        | Bool b :: s0 -> if b = true then  evaluate s t v (list_append c1 p0) else evaluate s t v (list_append c2 p0)
        | _ :: s0      ->                   evaluate [] ("Panic" :: t) v []
        | []           ->                   evaluate [] ("Panic" :: t) v []
      )
    | Bind :: p0 -> 
      (match s with 
        | Sym x :: var :: s0 ->             evaluate s0 t (((Sym x), var) :: v) p0
        | _ :: s0      ->                   evaluate [] ("Panic at bind" :: t) v []
        | []           ->                   evaluate [] ("Panic at bind" :: t) v []
      )
    | Lookup :: p0 -> 
      (match s with 
        | Sym x :: s0 ->  (match (lookup (Sym(x)) v) with
          | Some (value) ->                evaluate (value :: s0) t v p0 
          | None         ->                evaluate [] ("Panic at Lookup" :: t) v [])
        | _ :: s0        ->                evaluate [] ("Panic at Lookup" :: t) v []
        | []             ->                evaluate [] ("Panic at Lookup" :: t) v []     
      )
    | Fun c :: p0 -> 
      (match s with 
        | Sym x :: s0    ->                evaluate ((Closure (Sym x, v, c)) :: s0) t v p0
        | _ :: s0        ->                evaluate [] ("Panic at Fun" :: t) v []
        | []             ->                evaluate [] ("Panic at Fun" :: t) v []     
      )
    | Call :: p0 -> 
      (match s with 
        | Closure (cons, vprime, c) :: a :: s0 -> evaluate (a :: (Closure (Sym "cc", v, p0)) :: s0) t ((cons, (Closure(cons, vprime, c))) :: v) c
        | _ :: _ :: s0        ->                  evaluate [] ("Panic at Call" :: t) v []
        | _ :: s0             ->                  evaluate [] ("Panic at Call" :: t) v []
        | []                  ->                  evaluate [] ("Panic at Call" :: t) v []            
      )
    | Return :: p0 -> 
      (match s with 
        | Closure(f, vf, c) :: a :: s0->             evaluate (a :: s0) t vf c
        | _ :: _ :: s0        ->                evaluate [] ("Panic at Call" :: t) v []
        | _ :: s0             ->                evaluate [] ("Panic at Call" :: t) v []
        | []                  ->                evaluate [] ("Panic at Call" :: t) v []    
      )
    in
	match string_parse_c (parse_prog) s with 
	| Some (e, []) -> Some(evaluate([])([])([])(e)) 
	| _ -> None  
