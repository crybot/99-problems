(* Problem 14
 * Duplicate the elements of a list *)

let duplicate list =
  let compare elem acc = elem :: (elem :: acc) in
  List.fold_right compare list [];;
