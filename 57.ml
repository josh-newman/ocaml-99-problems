type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let construct l =
  let rec add tr v =
    match tr with
    | Empty -> Node (v, Empty, Empty)
    | Node (w, left, right) when v < w -> Node (w, add left v, right)
    | Node (w, left, right) -> Node (w, left, add right v)
  in
  List.fold_left add Empty l
;;

construct [3;2;5;7;1];;
