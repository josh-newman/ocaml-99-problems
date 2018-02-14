let rec last l =
  match l with
  | [] -> None
  | [e] -> Some e
  | e :: tail -> last tail
;;

last [];;

last ["a"; "b"; "c"];;