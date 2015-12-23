(* Problem 16
 * Drop every N'th element from a list *)

let drop list nth =
  let rec aux acc elem = function
    | [] -> acc 
    | hd::tl -> if hd = elem then aux acc elem tl
    else aux (hd::acc) elem tl
  in
  List.rev (aux [] (List.nth list (nth-1)) list);;

