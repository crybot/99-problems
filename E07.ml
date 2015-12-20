(* Problem 07
 * Flatten a nested list structure. (medium) *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten l = 
  match l with
  [] -> []
  | One x :: tl -> x :: (flatten tl) 
  | Many x :: tl -> flatten(x @ tl);;

