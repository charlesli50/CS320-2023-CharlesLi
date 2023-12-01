(* notes *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr> <exprs>
*)

(* operator -> argument *)

(*( add 1 2 (mul 3 4)) ==> 1 + 2 + (3 * 4) *)

#use "./../classlib/OCaml/MyOCaml.ml";;

type expr = 
  | Int of int
  | Add of expr list
  | Mul of expr list

let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ :: cs -> cs

let rec parse_digits cs = 
  match cs with 
  | [] -> ([], cs)
  | c :: cs -> 
    let x = ord c - ord '0' in
    if 0 <= x && x <= 9 then 
      let xs, cs = parse_digits cs in 
      (x :: xs, cs)
else ([], c :: cs)

let rec parse_int cs = 
  let xs, cs = parse_digits cs in
  match xs with 
  | [] -> None
  | _ -> let n = list_foldleft xs 0 (fun acc x -> acc * 10 + x) in
         Some (Int n, cs)

let parse_str s cs = 
  let cs0 = string_listize s in
  let rec loop cs cs0 = 
    match cs, cs0 with
    | c :: cs, c0:: cs0 ->
      if c = c0
      then loop cs cs0
    else None
    | _, [] -> Some cs
    | _ -> None
  in loop cs cs0


(* attempt to parse each parse, args: functions to run, remaining *)
let attempt ps cs =
  list_foldleft ps None (fun acc p -> 
    match acc with
    | Some _ -> acc
    | None -> p cs
    )

(* list of functions to attempt to call *)
let rec parse_expr cs = 
  attempt [parse_int; parse_add; parse_mul] cs
  (* attempt [parse_int; parse_add; parse_mul] cs *)

and parse_exprs cs = 
match parse_expr cs with
| None -> None
| Some (e, cs) -> 
  match parse_exprs(trim cs) with 
  | Some (es, cs) -> Some(e :: es, cs)
  | None -> Some ([e], cs)

and parse_add cs = 
match parse_str "(add" cs with 
| None -> None
| Some cs -> match parse_exprs(trim cs) with 
            | Some (es, ')':: cs) -> Some (Add es, cs)
            | _ -> None

and parse_mul cs = 
match parse_str "(mul" cs with 
| None -> None
| Some cs -> match parse_exprs(trim cs) with 
            | Some (es, ')':: cs) -> Some (Mul es, cs)
            | _ -> None

(* :) *)

let parse (s : string) : expr option = 
  let cs = string_listize s in
  match parse_expr cs with
  | Some (e, cs) -> Some e
  | _ -> None


