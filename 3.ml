let rec at idx l =
  match (idx, l) with
  | (_, []) -> None
  | (0, e :: _) -> Some e
  | (_, _ :: tail) -> at (idx - 1) tail
;;

at 3 [ "a" ];;

at 3 [ "a" ; "b"; "c"; "d"; "e" ];;