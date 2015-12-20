(* Problem 05
 * Reverse a list. (easy)
 *
 * OCaml standard library has List.rev but we ask that you reimplement it. *)

let rec rev l = match l with  
   [] -> []
  | [x] -> [x]
  | hd::tl -> (rev tl) @ [hd];;
  
