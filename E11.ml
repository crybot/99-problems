(* Problem 11
 * Modified run-length encoding. 
 * Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. 
 * Only elements with duplicates are transferred as (N E) lists.
 *
 * Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let how_many x count = 
  assert(count > 0);
  if count = 1 then One x else Many (count, x);;

let encode list = 
  let rec aux count acc = function
    | [] -> []
    | [x] -> (how_many x (count+1)) :: acc
    | a::(b::_ as t) -> 
        if a=b then aux (count+1) acc t
        else aux 0 (how_many a (count+1)::acc) t
  in
  aux 0 [] (List.rev list);;

