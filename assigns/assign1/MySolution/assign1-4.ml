(* Assignment 1-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let intrep_add (ds1:string) (ds2 :string): string = 

  let
  str2int(cs: string): int =
    string_foldleft(cs)(0)
    (fun r0 d0 -> 10 * r0 + digit_of_char(d0)) in 
  let int2str(n0: int): string =
    let rec foreach(n0: int) (work: char -> unit) = if n0 >= 10 then
      let d0 = char_of_digit(n0 mod 10) in (work(d0); foreach(n0/10)(work))
    else work(char_of_digit(n0))
  in
  string_rmake_fwork(foreach(n0))
  in
int2str ( str2int(ds1) + str2int(ds2) )
;;

(* intrep_add("1116123") ("222987");; *)