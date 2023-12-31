(* 
let rec stream_append(s1: 'a stream) (s2: 'a stream): 'a stream = fun () -> 

  match s1() with (* lazy evaluation here so execute with the parenthesis *)
  | StrNil -> s2()
  | StrCons(x1, xs) -> StrCons(x1, stream_append(xs)(s2))
*)


(*
let rec stream_zip2(s1: 'a stream) (s2: 'b stream): ('a * 'b) stream = fun () ->
  match (s1(), s2()) with
  | StrCons(x1, xs1), StrCons(x2, xs2) -> StrCons((x1, x2), stream_zip2(xs1)(xs2))
  | _ -> StrNil
*)

(* let rec stream_merge (s1: 'a stream) (s2: 'b stream): 'a stream = fun () ->
  match (s1(). s2()) with 
  | (StrCons(x1, xs1), StrCons(x2, xs2)) -> if x1 < x2 then StrCons(x1, stream_merge(xs1)(s2)) 
  else StrCons(x2, stream_merge(s1)(xs2)) 

  | (StrNil, s2) -> s2
  | (s1, StrNil) -> s1
  
  
  
  *)