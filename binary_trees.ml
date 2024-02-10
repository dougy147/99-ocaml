type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* Problem 55 : Construct completely balanced binary trees *)

let rec cbal_tree n =
  if n = 0 then [Empty] else
    let rec multiply_sides left right trees_list =
      List.fold_left (fun acc t -> List.fold_left (fun acc' t' -> Node ('x',t,t') :: acc' ) acc left) trees_list right
    in
    if n mod 2 = 0 then
      let sub1 = cbal_tree (n/2) and sub2 = cbal_tree (n/2-1) in
      multiply_sides sub2 sub1 (multiply_sides sub1 sub2 [])
    else
      let subtrees = cbal_tree (n/2) in
      multiply_sides subtrees subtrees []

      
(* Problem 56 : Symmetric binary trees *)

let rec is_mirror tree1 tree2 =
  match tree1, tree2 with
  | Node (_,l1,r1), Node (_,l2,r2) -> is_mirror l1 r2 && is_mirror l2 r1
  | Empty, Empty -> true
  | _ -> false

let is_symmetric tree = is_mirror tree tree

(* Problem 57 : Binary search trees (dictionnaries) *)


