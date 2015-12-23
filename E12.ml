(* Problem 12
 * Decode a run-length encoded list. 
 * Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let rec decode list = 
  let rec append = function
    | One elem -> [elem]
    | Many (0, elem) -> []
    | Many (times, elem) -> elem::append (Many (times-1, elem))
  in
  match list with
    | [] -> []
    | [x] -> append x
    | hd::tl -> (append hd) @ (decode tl);;

