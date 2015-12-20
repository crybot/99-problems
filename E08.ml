(* Problem 08
 * Eliminate consecutive duplicates of list elements. *)

 let rec aux acc list = match list with
    [] -> []
    | hd::tl -> if acc = [hd] then aux acc tl
    else hd :: (aux [hd] tl);;

 let compress = aux [] ;;

 (* Alternative solution using fold_right *)

 let compress_fold list = 
   let f x y =
     match y with
      [] -> [x]
      | hd::tl -> if x = hd then y else x :: y
   in
 List.fold_right f list [];;
