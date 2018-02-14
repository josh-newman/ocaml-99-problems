type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;

let layout_binary_tree_1 tr =
  let rec layout tr i d =
    match tr with
      | Empty -> (0, Empty)
      | Node (v, left, right) ->
        let (n_l, new_l) = layout left i (d + 1) in
        let (n_r, new_r) = layout right (i + n_l + 1) (d + 1) in
        ((n_l + 1 + n_r), Node ((v, i + n_l, d), new_l, new_r))
  in
  layout tr 1 1
;;

layout_binary_tree_1 example_layout_tree;;