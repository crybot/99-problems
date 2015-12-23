(* Problem 15
 * Replicate the elements of a list a given number of times *)

let replicate list n = 
  let rec f n elem acc = match n with
  | 1 -> elem :: acc
  | x -> elem :: f (x-1) elem acc
  in let g = f n in 
  List.fold_right g list [];;
