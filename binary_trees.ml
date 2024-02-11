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

let construct xs =
  let rec insert elem tree=
    match tree with
    | Empty -> Node (elem, Empty, Empty)
    | Node (x,l,r) ->
       if x = elem then tree else
       Node (x,(if elem < x then insert elem l else l),(if elem > x then insert elem r else r))
  in
  let rec aux xs tree =
    match xs with
    | [] -> tree
    | hd :: tl -> aux tl (insert hd tree)
  in
  aux xs Empty

(* Problem 58 : Generate-and-test paradigm *)

let sym_cal_trees n =
  List.filter (fun x -> is_symmetric x) (cbal_tree n)

(* Problem 59 : Construct height-balanced binary trees *)

let rec hbal_tree h =
  if h = 0 then [Empty] else
  if h = 1 then [Node('x',Empty,Empty)] else
    let rec multiply_sides left right tree =
      List.fold_left (fun acc r -> List.fold_left (fun acc' l -> Node ('x',l,r) :: acc') acc left) tree right
    in
    let diff1 = hbal_tree (h-1) and diff2 = hbal_tree (h-2) in
    multiply_sides diff1 diff2 (multiply_sides diff2 diff1 (multiply_sides diff1 diff1 []))


(* Problem 60 : Construct height-balanced binary trees with a given number of nodes *)

let max_nodes h = Int.of_float (2. ** (Float.of_int h) -. 1.)

(* I am discovering the Fibonnacci sequence in the construction of height-balanced trees with minimal number of nodes *)
                                  
let rec min_nodes h =
  if h = 1 then 1 else
  let fib n =
    let rec f a b i n =
      if i = n then a else
      f b (a+b) (i+1) n
    in
    f 0 1 0 n
  in
  min_nodes (h-1) + fib h

let min_height n = Int.of_float (ceil (Float.log2 (Float.of_int (n) +. 1.)))
