open Bin_tree;;

let rec insert_tree ord x = function
  | Empty -> Node (x, Empty, Empty)
  | Node (y, left, right) as t ->
      if ord x y then Node (y, insert_tree ord x left, right)
      else if ord y x then Node (y, left, insert_tree ord x right)
      else t
;;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l);;
