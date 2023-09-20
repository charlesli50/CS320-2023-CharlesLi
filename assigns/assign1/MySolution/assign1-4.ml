(* Assignment 1-4 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* 
let bad_intrep_add (ds1:string) (ds2 :string): string = 

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
;; *)

(* 
let rec
intrep_add_rec(larger: string)(smaller: string)(index: int)(carry: int)(work: char -> unit): unit =
if (string_length(smaller)) > index then
  let sum = carry + ord(string_get_at(larger)(index)) + ord(string_get_at(smaller)(index)) - 48 in 
    let carry = 0 in 
      (if sum > 57 then 
        let sum = sum - 10 in 
          let carry = 1 in 
            (work(chr(sum)); intrep_add_rec(larger)(smaller)(index+1)(carry)(work)) else 
              (work(chr(sum)); intrep_add_rec(larger)(smaller)(index+1)(carry)(work))) else
                      if string_length(larger) == index && carry == 1 then 
                        (work('1')) else (
                      if string_length(larger)-1 >= index then
                        if carry == 1 then 
                          let sum = carry + ord(string_get_at(larger)(index)) in
                          let carry = 0 in
                          (if sum > 57 then 
                            let sum = sum - 10 in 
                              let carry = 1 in 
                                (work(chr(sum)); intrep_add_rec(larger)(smaller)(index+1)(carry)(work)) else 
                                  (work(chr(sum)); intrep_add_rec(larger)(smaller)(index+1)(carry)(work))) else 
                          (work(string_get_at(larger)(index)); intrep_add_rec(larger)(smaller)(index+1)(carry)(work)))
;;

let
intrep_add(ds1: string)(ds2: string): string =
let a = stringrev(ds1) in
let b = stringrev(ds2) in
stringrev
(string_make_fwork 
    (fun work -> 
      (if string_length(a) > string_length(b) then intrep_add_rec(a)(b)(0)(0)(work) else intrep_add_rec(b)(a)(0)(0)(work)))) *)


let intrep_add (ds1:string) (ds2 :string): string = 
  let stringrev(cs:string): string =
  string_make_fwork(string_rforeach(cs)) in

  let rds1 = stringrev ds1 in
  let rds2 = stringrev ds2 in

  
  let rec addzeroes (len:int ) (str :string): string = 
    let strlen = string_length str in
    if len > strlen then
      let nextstr =  string_snoc str '0' in
      addzeroes len nextstr
    else str
  in

  let rec intrep_add_rec(ds1: string) (ds2: string)(index: int)(carry: int)(work: char -> unit): unit = 
    let len1 = string_length ds1 in
    let len2 = string_length ds2 in
    (* print_int len1; print_int len2; *)
    if(len1 > len2) then (* set both to same length *)
      (intrep_add_rec ds1 (addzeroes len1 ds2) 0 0 work)
    else if (len1 < len2) then 
      (intrep_add_rec (addzeroes len2 ds1) ds2 0 0 work)
    else 
      (* do the addition *)
    if (len2 > index) then(
      (* ( work(string_get_at(ds2)(index)); intrep_add_rec(ds1)(ds2)(index+1)(0) work) *)
      let nextSum = carry + (ord(string_get_at(ds1)(index)) + ord(string_get_at(ds2)(index))) - 48 in 
      if(nextSum > 57) then 
        let nextSum = nextSum - 10 in
        (work(chr(nextSum)); intrep_add_rec(ds1)(ds2)(index+1)(1) work)

      else 
        (work(chr(nextSum)); intrep_add_rec(ds1)(ds2)(index+1)(0) work)
    )
      else if (carry = 1) then work('1') else ()
  in
  (* stringrev (addzeroes 15 "Hello World") *)

  stringrev( string_make_fwork (fun work -> intrep_add_rec(rds1)(rds2)(0)(0)(work)  ))
  (* the carry bit = a char, you can just add to  *)

;;

intrep_add ("1116123") ("222987");;