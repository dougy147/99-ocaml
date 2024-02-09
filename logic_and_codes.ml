(* Problems 46 & 47 : Truth tables for logical expressions (2 variables) *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or  of bool_expr * bool_expr

(* And (Or (Var "a", Var "b"), And (Var "a", Var "b")) *)

let table2 a b expr =
  let rec parse_expr a abool b bbool expr =
    match expr with
    | Var x -> if x = a then abool else bbool (*consider user non error prone*)
    | And (x, y) -> parse_expr a abool b bbool x && parse_expr a abool b bbool y
    | Or  (x, y) -> parse_expr a abool b bbool x || parse_expr a abool b bbool y
    | Not x -> not (parse_expr a abool b bbool x)
  in
  (* let states = List.map (fun x -> List.map (fun y -> [x;y]) [true;false]) [true;false] |> List.flatten in *)
  let states = [(true,true);(true,false);(false,true);(false,false)] in
  List.map (fun (abool,bbool) -> (abool, bbool, parse_expr a abool b bbool expr)) states
  

(* Problem 48 : True tables for logical expressions. *)

let table var_list expr =
  let rec parse_expr var_bools expr =
    match expr with
    | And (x, y) -> parse_expr var_bools x && parse_expr var_bools y
    | Or  (x, y) -> parse_expr var_bools x || parse_expr var_bools y
    | Not x -> not (parse_expr var_bools x)
    | Var x -> match List.filter (fun (v,vbool) -> v = x) var_bools with (*should have used List.assoc instead of filter*)
               | [(_,varbool)] -> varbool
               | _ -> failwith "Error with variable"
    (*why if I put the pattern-match "Var x" before every other patterns I get an error?*)
  in
  let rec generate_states n =
    if n = 1 then [[true];[false]] else
      List.map (fun x -> List.map (fun y -> x @ y) (generate_states (n-1)) ) (generate_states 1)
      |> List.flatten
  in
  let bool_states = generate_states (List.length var_list) in
  let states = List.map (fun x -> List.combine var_list x) bool_states in
  List.map (fun x -> (x, parse_expr x expr)) states

(* Problem 49:  Gray code *)
let rec gray n =
  if n = 1 then ["0";"1"] else
    List.map (fun x ->
        if x = "1" then
          List.rev (List.map (fun y -> x ^ y) (gray (n-1)))
        else
          List.map (fun y -> x ^ y) (gray (n-1))) (gray 1)
    |> List.flatten
    
(* Problem 50: Huffman code *)

let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]

(* https://en.wikipedia.org/wiki/Huffman_coding#Basic_technique *)
type node =
  | Leaf of float * string (* (proba, letter) *)
  | Node of float * (node * node)

let huffman xs = 
  let freq_to_prob xs =
    let sum = List.fold_left (fun s (_,x) -> x + s) 0 xs in
    List.map (fun (c,freq) -> Leaf (Float.of_int freq /. Float.of_int sum, c) ) xs
  in
  let compare_nodes n1 n2 = (*adapted implementation of "compare" function *)
    match n1 with
    | Leaf (p1,_) | Node (p1,_) ->
       match n2 with
       | Leaf (p2,_) | Node (p2,_) -> if p1 = p2 then 0 else if p1 > p2 then -1 else 1 (* reversed order *)
  in
  let queue = List.rev (List.sort compare_nodes (freq_to_prob xs)) in
  let rec build_tree xs =
    match xs with
    | [] -> failwith "Empty priority queue"
    | [x] -> x
    | x :: y :: tl ->
       match x with
       | Leaf (p1,_) | Node (p1,_) ->
          match y with
          | Leaf (p2,_) | Node (p2,_) ->
             let nnode  = Node (p1 +.p2, (x,y)) in
             let nqueue = List.rev (List.sort compare_nodes (nnode :: tl)) in
             build_tree nqueue
  in
  let tree = build_tree queue in
  let rec label_tree label tree =
    match tree with
    | Leaf (_,x) -> [(x,label)]
    | Node (_,(x,y)) -> (label_tree (label ^ "0") x) @ (label_tree (label ^ "1") y)
  in
  label_tree "" tree

(* testing with Wikipedia's example "this is an example of a huffman tree". Is lexicographic order important? *)
(* let wiki_ex = [(" ",7); ("a",4); ("e",4); ("f",3); ("h",2); ("i",2); ("m",2); ("n",2); ("s",2); ("t",2); ("l",1); ("o",1); ("p",1); ("r",1); ("u",1); ("x",1)] *)
(* Different Huffman encoding can be valid for the same input *)
