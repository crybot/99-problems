(* Problem 02
 * Find the last but one (last and penultimate) elements of a list. *)

let rec last_two l = 
  match l with
  [] -> None
  | [x] -> None 
  | hd :: [tl] -> Some (hd, tl)
  | hd :: tl -> last_two tl ;;

