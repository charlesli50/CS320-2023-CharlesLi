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

type com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | IfElse of (com list*com list)
(* and coms = com list *)

(* let valid_first_char ( c: char) = 
 *)


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
    pure (IfElse (c1, c2) )(ls))

let parse_prog = many (parse_com() << keyword ";")

let string_parse_c(p: 'a parser)(s: string) =
  p(string_listize(s))

let interp (s: string) = 
  let rec evaluate(s : const list) (t : string list) (p : com list) : string list =
    match p with
    (* termination state returns the trace *)
    | [] -> t
    | Push c :: p0 (* PushStack *) -> evaluate (c :: s) t p0
    | Swap :: p0 -> 
      (match s with
      | i :: j :: s0   (*swap stack*) -> evaluate ( j :: i :: s0) t p0
      | _ :: []        (* Die *)      -> evaluate [] ("Panic" :: t) []
      | []                            -> evaluate [] ("Panic" :: t) []
      )
    | Pop :: p0 ->
      (match s with
       | _ :: s0 (* PopStack *) -> evaluate s0 t p0
       | []      (* PopError *) -> evaluate [] ("Panic" :: t) [])
    | Trace :: p0 ->
      (match s with
       | c :: s0 (* TraceStack *) -> evaluate (Unit  :: s0) (const_to_string c :: t) p0
       | []      (* TraceError *) -> evaluate [] ("Panic" :: t) [])
    | Add :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* AddStack *)  -> evaluate (Int (i + j) :: s0) t p0
       | _ :: _ :: s0         (* AddError1 *) -> evaluate [] ("Panic" :: t) []
       | []                   (* AddError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []              (* AddError3 *) -> evaluate [] ("Panic" :: t) [])
    | Sub :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* SubStack *)  -> evaluate (Int (i - j) :: s0) t p0
       | _ :: _ :: s0         (* SubError1 *) -> evaluate [] ("Panic" :: t) []
       | []                   (* SubError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []              (* SubError3 *) -> evaluate [] ("Panic" :: t) [])
    | Mul :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* MulStack *)  -> evaluate (Int (i * j) :: s0) t p0
       | _ :: _ :: s0         (* MulError1 *) -> evaluate [] ("Panic" :: t) []
       | []                   (* MulError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []              (* MulError3 *) -> evaluate [] ("Panic" :: t) [])
    | Div :: p0 ->
      (match s with
       | Int i :: Int 0 :: s0 (* DivError0 *) -> evaluate [] ("Panic" :: t) []
       | Int i :: Int j :: s0 (* DivStack *)  -> evaluate (Int (i / j) :: s0) t p0
       | _ :: _ :: s0         (* DivError1 *) -> evaluate [] ("Panic" :: t) []
       | []                   (* DivError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []              (* DivError3 *) -> evaluate [] ("Panic" :: t) [])
    | And :: p0 ->
      (match s with
       | Bool a :: Bool b :: s0 (* AndStack *)  -> evaluate (Bool (a && b) :: s0) t p0
       | _ :: _ :: s0           (* AndError1 *) -> evaluate [] ("Panic" :: t) []
       | []                     (* AndError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []                (* AndError3 *) -> evaluate [] ("Panic" :: t) [])
    | Or :: p0 ->
      (match s with
       | Bool a :: Bool b :: s0 (* OrStack *)  -> evaluate (Bool (a || b) :: s0) t p0
       | _ :: _ :: s0           (* OrError1 *) -> evaluate [] ("Panic" :: t) []
       | []                     (* OrError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []                (* OrError3 *) -> evaluate [] ("Panic" :: t) [])
    | Not :: p0 ->
      (match s with
       | Bool a :: s0 (* NotStack  *) -> evaluate (Bool (not a) :: s0) t p0
       | _ :: s0      (* NotError1 *) -> evaluate [] ("Panic" :: t) []
       | []           (* NotError2 *) -> evaluate [] ("Panic" :: t) [])
    | Lt :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* LtStack *)  -> evaluate (Bool (i < j) :: s0) t p0
       | _ :: _ :: s0         (* LtError1 *) -> evaluate [] ("Panic" :: t) []
       | []                   (* LtError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []              (* LtError3 *) -> evaluate [] ("Panic" :: t) [])
    | Gt :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* GtStack *)  -> evaluate (Bool (i > j) :: s0) t p0
       | _ :: _ :: s0         (* GtError1 *) -> evaluate [] ("Panic" :: t) []
       | []                   (* GtError2 *) -> evaluate [] ("Panic" :: t) []
       | _ :: []              (* GtError3 *) -> evaluate [] ("Panic" :: t) [])
    | IfElse (c1, c2) :: p0 ->       
      (match s with 
        | Bool b :: s0 -> if b = true then evaluate s t (list_append c1 p0) else evaluate s t (list_append c2 p0)
        | _ :: s0      ->                   evaluate [] ("Panic" :: t) []
        | []           ->                   evaluate [] ("Panic" :: t) []
      )
    in
	match string_parse_c (parse_prog) s with 
	| Some (e, []) -> Some(evaluate([])([])(e)) 
	| _ -> None  
