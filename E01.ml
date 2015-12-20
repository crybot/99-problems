(* Problem 01
 * Write a function last : 'a list -> 'a option that returns the last element of a list. *)

let last l = 
  let length = List.length l in
  match l with
  [] -> None
  | _ -> Some (List.nth l (length -1));;
