#use "MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

123
(add 123 123)
(add (mul 1 2) 123)

((add 123 123)
(123 123)

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let string_of_list (cs : char list) : string =
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

let rec parse_expr () : expr parser =
  parse_int () <|> parse_add () <|> parse_mul ()

and parse_int () : expr parser =
  let* n = natural in
  pure (Int n) << whitespaces

and parse_add () : expr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (Add es)

and parse_mul () : expr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (Mul es)

let parse (s : string) : expr option =
  match string_parse (parse_expr ()) s with
  | Some (e, []) -> Some e
  | _ -> None
