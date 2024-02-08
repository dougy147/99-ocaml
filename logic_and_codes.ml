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
(* order is important *)
let rec gray n =
  if n = 1 then ["0";"1"] else
    List.map (fun x ->
        if x = "1" then
          List.rev (List.map (fun y -> x ^ y) (gray (n-1)))
        else
          List.map (fun y -> x ^ y) (gray (n-1))) (gray 1)
    |> List.flatten
    
