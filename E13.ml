(* Problem 13
 * Run-length encoding of a list (direct solution). (medium)
 *
 * Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X. *)

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

