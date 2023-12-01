(* #use "./../../../classlib/OCaml/MyOCaml.ml";;
(* check if all increasing, all decreasing or more than 2 numbers *)
let string_avoid_132(cs: string): bool = 
  let len = string_length cs 
  in
  let rec recursive (cs:string) (i0:int) (i1:int) (i2:int): bool = 
    let char0 = string_get_at cs i0 in
    let char1 = string_get_at cs i1 in
    let char2 = string_get_at cs i2 in
    if(
      i0 < i2 && i2 < i1 && char0 < char2 && char2 < char1
    ) then false
    
    (* recursive stuff *)
    else
    if i0 = len && i1 = len && i2 = len then 
      true
    else 
      if i0 < len-1 then 
        recursive cs (i0+1) (0) (0) 
    else 
      if i1 < len-1 then 
        recursive cs (i0) (i1+1) (0)
    else 
      if i2 < len-1 then 
        recursive cs (i0) (i1) (i2+1)
      else (print_endline"badfalse";false)
    (* else false *)
in 

recursive cs 0 0 0
;; *)