(* Problem 10
 * Run-length encoding of a list *)

let encode l=
  let rec aux count acc = function
    [] -> []
    | [x] -> (count+1, x) :: acc
    | a::((b::t) as tl) -> if a=b then aux (count+1) acc tl else aux 0 ((count+1,a)::acc) tl
  in
  aux 0 [] (List.rev l);
