(* Assignment 2-4 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

(* string work make iforeach string for each do work on it*)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;


let
string_sepjoin_list(sep: string)(xs: string list): string =
let res = string_make_fwork(
  fun work -> list_foreach xs (fun cs -> (string_foreach cs work; string_foreach sep work))
) in   
let truncate_last_sep (sep: string) (str: string): string = 
  let str_len = string_length str in
  let sep_len = string_length sep in
  let rec truncate_last_sep_rec (str: string) (index: int) (len: int) (work): unit = 
    if (len > index) then
      (work(string_get_at(str)(index)); truncate_last_sep_rec str (index+1) len work)
    else ()
  in
  string_make_fwork(truncate_last_sep_rec str 0 (str_len - sep_len))
in
truncate_last_sep sep res
;;

let s0 = string_sepjoin_list(",,")(["1";"1";"1"]);;
let s1 = string_sepjoin_list(",")([]);;