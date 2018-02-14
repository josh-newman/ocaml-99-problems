let rec rotate l n =
  match (l, n) with
  | (_, 0) -> l
  | ([], _) -> []
  | (e :: tail, _) -> List.append (rotate tail (n - 1)) [e]
;;

rotate [1; 2; 3] 1;;
