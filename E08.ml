(* Problem 08
 * Eliminate consecutive duplicates of list elements. *)

 let rec aux acc list = match list with
    [] -> []
    | hd::tl -> if acc = [hd] then aux acc tl
    else hd :: (aux [hd] tl);;

 let compress = aux [] ;;
