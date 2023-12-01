#use "MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let string_of_list (cs) =
  string_make_fwork(fun work -> list_foreach cs work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)

type 'a parser = char list -> ('a * char list) option

let parse_digit (cs : char list) : (int * char list) option =
  match cs with
  | [] -> None
  | c :: cs ->
    let i = ord c - ord '0' in 
    if 0 <= i && i <= 9
    then Some (i, cs)
    else None

let rec many (p : 'a parser) (cs : char list) : ('a list * char list) option =
  match p cs with
  | Some (x, cs0) ->
    (match many p cs0 with
     | Some (xs, cs1) -> Some (x :: xs, cs1)
     | None -> Some ([x], cs0))
  | None -> Some ([], cs)

let rec many1 (p : 'a parser) (cs : char list) : ('a list * char list) option =
  match p cs with
  | Some (x, cs0) ->
    (match many p cs0 with
     | Some (xs, cs1) -> Some (x :: xs, cs1)
     | None -> None)
  | None -> None

let int_of_list xs =
  let rec loop xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> loop xs (acc * 10 + x)
  in
  loop xs 0

let parse_num (cs : char list) : (int * char list) option =
  match many1 parse_digit cs with
  | Some (xs, cs0) -> Some (int_of_list xs, trim cs0)
  | None -> None

let parse_word (s : string) (cs : char list) : (unit * char list) option =
  let cs0 = string_listize s in
  let rec loop cs cs0 =
    match cs, cs0 with
    | _, [] -> Some ((), trim cs)
    | [], _ -> None
    | c :: cs', c0 :: cs0' ->
      if c = c0
      then loop cs' cs0'
      else None
  in
  loop cs cs0

let rec choice (ps : 'a parser list) (cs : char list) : ('a * char list) option = 
  match ps with
  | [] -> None
  | p :: ps0 ->
    match p cs with
    | Some (x, cs0) -> Some (x, cs0)
    | None -> choice ps0 cs

let rec parse_expr (cs : char list) : (expr * char list) option =
  choice [parse_int; parse_add; parse_mul] cs

and parse_int (cs : char list) : (expr * char list) option =
  match parse_num cs with
  | Some (i, cs0) -> Some (Int i, cs0)
  | None -> None

and parse_add (cs : char list) : (expr * char list) option =
  match parse_word "(add" cs with
  | Some (_, cs0) -> 
    (match many1 parse_expr cs0 with
     | Some (es, cs1) ->
       (match parse_word ")" cs1 with
        | Some (_, cs2) -> Some (Add es, cs2)
        | None -> None)
     | None -> None)
  | None -> None

and parse_mul (cs : char list) : (expr * char list) option =
  match parse_word "(mul" cs with
  | Some (_, cs0) -> 
    (match many1 parse_expr cs0 with
     | Some (es, cs1) ->
       (match parse_word ")" cs1 with
        | Some (_, cs2) -> Some (Mul es, cs2)
        | None -> None)
     | None -> None)
  | None -> None


let parse (s : string) : expr option =
  match parse_expr (string_listize s) with
  | Some (e, []) -> Some e
  | _ -> None
