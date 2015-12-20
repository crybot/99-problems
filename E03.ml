(* Problem 03
 * Find the kth element of a list *)

let rec at k l = match (l,k) with
  ([],_) -> None
  | (hd::tl, 1) -> Some hd
  | (hd::tl, _) -> at (k-1) tl;;
