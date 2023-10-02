(* Assignment 3-1 -- cli50@bu.edu -- for CS320 *)
(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec 
matrix_transpose(xss: 'a list list): 'a list list = 
 
;;


(*  let rec list_get(xs: 'a list)(i0: int): 'a = 
    match xs with
    | [] -> failwith "nan"
    | x1 :: xs ->
      if i0 = 0 then x1
      else list_get xs (i0 - 1)
  in

  let rec get_col (xss: 'a list list)(c0: int): 'a list = 
    list_make_fwork(fun work -> 
      let col = list_foldleft(xss)([])(fun r0 x0 -> 
        (list_append(r0)(list_get(x0)(c0)))
        (* r0 :: list_get(x0)(c0) *)
        (* r0 + 1 *)
      )
      in
      work(col)
    )
  in  
  (* (get_col(xss)(1);
  [[]]) *)
  [get_col(xss)(1)]
;;

let m1 =
  [[1;2;3];[4;5;6];[7;8;9]]
  ;;

let n1 = matrix_transpose(m1) *)
  (* list_make_fwork(
    fun work -> 
    let _ = list_foldleft(xs)([])
    in 
    work(1)
  ) *)

(* let list_concat(xss: 'a list list): 'a list = 
  list_make_fwork(
    fun work -> list_foreach xss (fun xs -> list_foreach xs work)
  )
;; *)



(* 
 let rec matrix_transpose xss =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ -> List.map List.hd xss :: matrix_transpose (List.map List.tl xss) *)
  

(* type
('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

type
('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

(* zip the matrices into lists, then add together *)
let foldleft_to_iforeach(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  (* let foldleft_to_iforeach(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =  *)
    fun xs work -> 
      let _ = foldleft xs 0 (fun idx x0 -> (work(idx)(x0); idx+1)) in 
      ()
  in

  let list_iforeach = fun xs -> foldleft_to_iforeach(list_foldleft)(xs) in

  list_make_fwork(
    fun work -> 
      list_iforeach (xss) (fun i x -> work(i, x))
  )
   *)