(* Assignment 1-5 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;


let string_longest_ascend (xs:string): string =
  (* let prevChar = string_get_at(0) in *)
  (* if char is greater than previous character, true, else false *)

  let rec string_longest_ascend_rec (xs: string) (index: int) (current_longest: string): string = 
    let strlen = string_length xs in

    let rec string_iter_helper (xs:string) (s_index: int) (c_index:int )(prev_char: char)(work: char -> unit): unit = 
      if(s_index = c_index) then 
        (work(string_get_at xs c_index); string_iter_helper(xs) (s_index) (c_index+1) (string_get_at xs s_index) (work))
      else if (c_index < string_length xs) then
        if (string_get_at xs c_index) < prev_char then
          string_iter_helper(xs) (s_index) (c_index+1) (prev_char) (work)
        else 
        (work(string_get_at xs c_index); string_iter_helper(xs) (s_index) (c_index+1) (string_get_at xs c_index) (work))
        
    in
    if strlen > index then (*outside loop, for each starting integer*)
      (* start checking if from current index you have the longest string *)
      let current_string = string_make_fwork(fun work -> string_iter_helper(xs) (index) (index) (string_get_at (xs) (index)) (work)) in

      if(string_length current_string > string_length current_longest) then 
        string_longest_ascend_rec xs (index+1) current_string
      else 
        string_longest_ascend_rec xs (index+1) current_longest

    else current_longest

  in
  string_longest_ascend_rec xs 0 ""
;;


string_longest_ascend "12345";;
string_longest_ascend "1324561111";;
string_longest_ascend "1234561111";;
string_longest_ascend "1234511111";;
string_longest_ascend "abcde";;