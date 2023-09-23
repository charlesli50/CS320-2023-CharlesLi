(* Assignment 2-4 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let string_sepjoin_list (sep: string)(xs: string list) : string =
  let rec string_sepjoin_list_rec (sep: string)(xs: string list) : string list = 
  match xs with
  | [] -> []
  | x1 :: xs -> (x1 :: sep :: string_sepjoin_list_rec(sep) (xs))

  (* xs *)
in
  let result = string_concat_list(string_sepjoin_list_rec (sep) (xs))
in
if string_length (result) = 0 then 
  ""
else
  let truncate_last_sep (sep: string) (str: string): string = 
    let str_len = string_length str in
    let sep_len = string_length sep in
    let rec truncate_last_sep_rec (str: string) (index: int) (len: int) (work): unit = 
      (* print_int index; *)
      if (len > index) then
        (* () *)
        (work(string_get_at(str)(index)); truncate_last_sep_rec str (index+1) len work)
      else ()
    in
    string_make_fwork(truncate_last_sep_rec str 0 (str_len - sep_len))
  in
truncate_last_sep sep result

;;

let s0 = string_sepjoin_list(",,")(["1";"22";"333"]);;
let s1 = string_sepjoin_list(",")([]);;