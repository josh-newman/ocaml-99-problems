let rec compress l =
  match l with
  | [] -> []
  | [e] -> [e]
  | e :: f :: tail when e = f -> compress (f :: tail)
  | e :: f :: tail -> e :: (compress (f :: tail))
;;


compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;