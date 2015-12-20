(* Problem 09
 * Pack consecutive duplicates of list elements into sublists. *)

let pack = 
  let rec aux acc list = match list with
    [] -> [acc]
    | hd::tl when acc <> [] -> if List.hd acc = hd then aux (hd::acc) tl 
    else acc :: aux [hd] tl 
    | hd::tl -> aux ([hd]) tl
  in
  aux [] ;;


