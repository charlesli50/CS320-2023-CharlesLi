(* Assignment 0-5 -- cli50@bu.edu -- for CS320 *)

let rec stringrev(cs: string): string =
  let rec getChar(i: int): char = 
    String.get cs i
  in

  let len = String.length cs in
  String.init len (fun i -> getChar(len - i - 1))

;;

stringrev "Hello World";;