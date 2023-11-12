(* Assignment 6-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)

type sexpr =
  | SInt of int
  | SAdd of sexpr list
  | SMul of sexpr list


(* ****** ****** *)


let rec sexpr_to_string (e : sexpr) : string  =
  let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs) in
  let concat_with_whitespaces (l : string list) : string = 
    list_foldleft (list_reverse l) ("") (fun acc x -> 
        let res = string_append x acc
        in string_append " " res
      )
    in
  match e with
  | SInt i -> string_of_int i
  | SAdd lst -> let args = concat_with_whitespaces(list_map lst sexpr_to_string) in
    string_append (string_append "(add" args) ")"
  | SMul lst -> let args = concat_with_whitespaces(list_map lst sexpr_to_string) in
    string_append (string_append "(mul" args) ")"

(* type 'a parser = char list -> ('a * char list) option *)

let rec parse_expr () : sexpr parser =
  parse_int () <|> parse_add () <|> parse_mul ()

and parse_int () : sexpr parser =
  let* n = natural in
  pure (SInt n) << whitespaces

and parse_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SAdd es)

and parse_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SMul es)

let sexpr_parse (s : string) : sexpr option = 
  match string_parse (parse_expr ()) s with
  | Some (e, []) -> Some e
  | _ -> None