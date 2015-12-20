(* Problem 4
 * Find the number of elements of a list. (easy)
 *
 * OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)

let rec length_helper accu l = match l with
  [] -> accu
  | hd::tl -> length_helper (accu + 1) tl;;

let length l = length_helper 0 l;;
