(* Assignment 4-3 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* ****** ****** *)

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

(* ****** ****** *)

let
fchildren
(x0: 'a gtree): 'a gtree list =
match x0 with
| GTnil -> [] | GTcons(_, xs) -> xs

(* ****** ****** *)

let rec
gtree_dfs
( nxs
: 'b list)
( fchildren
: 'b -> 'b list): 'b stream = fun() ->
(
match nxs with
  [] -> StrNil
| nx1 :: nxs ->
  StrCons(nx1, gtree_dfs(fchildren(nx1) @ nxs)(fchildren))
)
;;

let rec streamize(resStream) = fun() ->
  match resStream() with
  | StrNil -> StrNil
  | StrCons(x, xs) ->
    match x with
    |GTnil -> streamize(xs)()
    |GTcons(x1, rest) -> StrCons(x1, streamize(xs))


let rec 
gtree_streamize_dfs(xs: 'a gtree): 'a stream = 
fun() -> streamize(gtree_dfs([xs])(fchildren))()



let rec
gtree_bfs
( nxs
: 'b list)
( fchildren
: 'b -> 'b list): 'b stream = fun() ->
(
match nxs with
  [] -> StrNil
| nx1 :: nxs ->
  StrCons(nx1, gtree_bfs(nxs @ fchildren(nx1))(fchildren))
)
;;

let rec 
gtree_streamize_bfs(xs: 'a gtree): 'a stream = 
fun() -> streamize(gtree_bfs([xs])(fchildren))()

    