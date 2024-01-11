(* https://ocaml.org/exercises *)
(* ERROR PRONE: https://v2.ocaml.org/learn/tutorials/99problems.html *)

(* Working with lists *)

(* Problem 01 : Tail of a List *)

let rec last xs =
  match xs with
  | [] -> None
  | [x] -> Some x
  | h :: t ->
      last t

(* Problem 02 : Last Two Elements of a List *)

let rec last_two xs =
  match xs with
  | [] -> None
  | [x;y] -> Some (x,y)
  | h :: t -> last_two t

(* Problem 03 : N'th Element of a List *)

let rec nth xs index =
  match xs with
  | [] -> None
  | h :: _ when index = 0 -> Some h
  | h :: t -> nth t (index - 1)

(* Official answer :
let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t;;
What is interesting is that we are not even forced to prepare an argument for the list.
It suffice to call it with : at 2 [1;2;3;4];;
*)

(* Problem 04 : Length of a List *)
let length list =
  let rec l acc list =
    match list with
    | [] -> acc
    | _ :: t -> l (acc + 1) t
  in
  l 0 list

(* Should do the same as: *)
(* let length list =
  let rec l acc = function (* function is right-associative *)
    | [] -> acc
    | _ :: t -> l (acc + 1) t
  in
  l 0 list
*)

(* Problem 05 : Reverse a List *)

let rev list =
  let rec r reversed list =
    match list with
    | [] -> reversed
    | h :: t -> r (h :: reversed) t
  in
  r [] list

(* Problem 06 : Palindrome *)

let palindrome list =
  list = rev list

(* Problem 07 : Flatten a nested list structure *)

(* we create a new type called 'node' that can be 'One' or 'Many' *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten list =
  let rec f acc list =
    match list with
    | One x :: rest -> f (x :: acc) rest
    | Many x :: rest -> f acc (x @ rest)
    | _ -> rev acc
  in
  f [] list

(* Problem 08 : Eliminate Consecutive Duplicates *)
let compress list =
  let rec c acc list =
    match list with
    | [] -> acc
    | [x] -> x :: acc
    | x :: y :: rest ->
        if x = y then
          c acc (y :: rest)
        else
          c (x :: acc) (y :: rest)
  in
  rev (c [] list)

(*  (* From the official example *)
let rec compress2 list =
  match list with
  | first :: (second :: _ as t) ->
      if first = second then
        compress2 t
      else
        first :: compress2 t
  | smaller -> smaller (*smaller means any value that hasn't been matched in the previous ones. It is an identifier*)
*)

(* Problem 09 : Pack consecutive duplicates into sublists*)

let pack list =
  let rec p acc acc2 list =
    match list with
    | [] -> acc2 @ [acc]
    | hd :: tl ->
        match acc with
        | [] -> p (hd :: acc) acc2 tl
        | hd2 :: _ ->
            if hd = hd2 then
              p (hd :: acc) acc2 tl
            else
              p [hd] (acc2 @ [acc]) tl
  in
  p [] [] list

(* Problem 10 : Run-length encoding of a List *)

let encode list =
  let rec e acc list =
    match list with
    | [] | [] :: _ -> acc
    | (h::r) :: rest -> e ((length (h::r), h)::acc) rest
  in
  e [] (rev (pack list))

(* Problem 11 : Modified run-length encoding*)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode list =
  let rec modif acc list =
    match list with
    | [] -> acc
    | (x,y) :: rest when x > 1 -> modif ((Many (x,y))::acc) rest
    | (x,y) :: rest -> modif ((One y)::acc) rest
  in
  let rec e acc list =
    match list with
    | [] | [] :: _ -> rev (modif [] acc)
    | (h::r) :: rest -> e ((length (h::r), h)::acc) rest
  in
  e [] (rev (pack list))

(* Problem 12 : Decode a run-length encoded list *)
(* decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];; *)

let decode list =
  let rec unwrap acc index symbol =
    match index with
    | 1 -> (symbol::acc)
    | _ -> unwrap (symbol::acc) (index-1) symbol
  in
  let rec d acc list =
    match list with
    | [] -> rev acc
    | Many (x,y) :: rest -> d ((unwrap [] x y)@acc) rest
    | One x :: rest -> d (x::acc) rest
  in
  d [] list

(* Problem 13 : Run-length encoding of a list (direct solution) *)
(* i.e. not storing the list of duplicates but immediately counting. *)

let encode list =
  let count index symbol = if index = 1 then (One symbol) else (Many (index,symbol)) in
  (*let rec label acc list = (*what first came to mind. non-optimal, right?*)
    match list with
    | [] -> acc
    | (l,x)::tl when l > 1 -> label ((Many(l,x))::acc) tl
    | (l,x)::tl -> label ((One x)::acc) tl
  in*)
  let rec e acc index list =
    match list with
    | [] -> acc
    | hd :: [] -> (count index hd::acc)
    | first::second::tail ->
        if first = second then
          e acc (index+1) (second::tail)
        else
          e ((count index first)::acc) 1 (second::tail)
  in
  rev (e [] 1 list)


(* Problem 14 : Duplicate the elements of a list *)

let duplicate list =
  let rec dup acc = function
    | [] -> acc
    | hd :: tl -> dup (hd::hd::acc) tl
  in
  List.rev (dup [] list)

(* non tail recursive solution *)
(*let rec duplicate = function
  | [] -> []
  | hd :: tl -> hd::hd::(duplicate tl)*)

(* Problem 15 : Replicate the elements of a list a given number of times *)

let replicate list repeat=
  let rec mult acc index item =
    if index = 0 then acc else mult (item::acc) (index-1) item
    (* match index with
    | 0 -> acc
    | _ -> mult (item::acc) (index-1) item *)
  in
  let rec r list repeat =
    match list with
    | [] -> []
    | hd :: tl -> (mult [] repeat hd) @ (r tl repeat)
  in
  r list repeat

(* Problem 16 : Drop every N'th element from a list *)

let drop list n =
  let rec d original_n n = function
    | [] -> []
    | head :: tail -> if n = 1 then d original_n original_n tail else head::(d original_n (n-1) tail)
  in
  d n n list
(* Original solution is better with index i: if i = n ... *)


(* Problem 17 : Split a List Into Two Parts; The Length of the First Part Is Given *)

let split list length =
  let rec s acc list l =
    match list with
    | [] -> (List.rev acc,[])
    | hd :: tl as initlist -> if l = 0 then (List.rev acc,initlist) else s (hd::acc) tl (l-1)
  in
  s [] list length

(* Problem 18 : Extract a slice from a list *)

let slice list i k =
  let rec s acc list i range =
    match i,range with
    | 0,0 -> List.rev (List.hd list :: acc)
    | 0,_ -> if List.tl list = [] then
               List.rev (List.hd list :: acc)
             else
               s (List.hd list::acc) (List.tl list) i (range-1)
    | _ -> s acc (List.tl list) (i-1) range
  in
  if k < i || i >= length list then [] else s [] list i (k-i)

(* Problem 19 : Rotate a list N places to the left *)

let rotate list n =
  let rec r acc n list =
    if n = 0 then list @ (List.rev acc) else r (List.hd list::acc) (n-1) (List.tl list)
  in
  if List.length list = 0 then [] else r [] (n mod (List.length list)) list

(* Problem 20 : Remove the K'th element from a list *)

let remove_at k list =
  let rec r_at acc k = function
    | [] -> List.rev acc
    | hd :: tl -> if k = 0 then (List.rev acc) @ tl else r_at (hd::acc) (k-1) tl
  in
  r_at [] k list
(* More performant than the example's solution *)

(* Problem 21 : Insert an Element at a Given Position Into a List *)

let insert_at element index list =
  let rec aux acc elem index = function (* starting to use the name 'aux' that seems common use? *)
    | [] -> (List.rev acc) @ [elem]
    | l when index = 0 -> (List.rev acc) @ [elem] @ l
    | hd :: tl -> aux (hd :: acc) elem (index-1) tl
  in
  aux [] element index list

(* Problem 22 : Create a List Containing All Integers Within a Given Range *)
(* not clear what is wanted if a > b *)

let range a b =
  let rec aux acc lower diff =
    if diff = -1 then acc else aux (lower+diff::acc) lower (diff-1)
  in
  if a <= b then aux [] a (b-a) else List.rev (aux [] b (a-b))


(* Problem 23 : Extract a Given Number of Randomly Selected Elements From a List  *)
(* Not explicit if draw with or without replacement. *)

exception Unreachable of string;;
(* with replacement *)
let rand_select list number =
  Random.init 0;
  let rec nth index = function
    | [] -> raise (Unreachable "Uh, unreachable no?")
    | hd :: tl -> if index = 0 then [hd] else nth (index-1) tl
  in
  let rec aux acc list n =
    if n = 0 then acc else aux ((nth (Random.int (List.length list)) list) @ acc) list (n-1)
  in
  aux [] list number

(* without replacement *)
let rand_select list number =
  Random.init 0;
  let rec pop acc index list =
    match list with
    | [] -> raise (Unreachable "Uh, unreachable no?")
    | hd :: tl ->
        if index = 0 then
          hd,acc @ tl
        else
          pop (hd::acc) (index-1) tl
  in
  let rec aux acc list number =
    match list with
    | [] -> acc
    | hd :: tl ->
        if number = 0 then
          acc
        else
          let popped,remaining = pop [] (Random.int (List.length list)) list in
          aux (popped :: acc) remaining (number-1)
  in
  aux [] list number

(* Problem 24 : Draw N Different Random Numbers From the Set 1..M *)
(* There seems to be a trade-off between those two functions *)

(* "Good" for small n but large m *)
let lotto_select n m =
  let rec is_in_acc list number =
    match list with
    | [] -> false
    | hd :: _ when number = hd -> true
    | hd :: tl -> is_in_acc tl number
  in
  let rec aux acc n m =
    if List.length acc = n then
      acc
    else
      let rn = (Random.int m) + 1 in
      if is_in_acc acc rn then aux acc n m else if n = 0 then acc else aux (rn::acc) (n-1) m
  in
  aux [] n m

(* "Good" for large m but small n *)
let lotto_select n m = rand_select (range 1 m) n

(* Problem 25 : Generate a Random Permutation of the Elements of a List *)

(* If we remove "Random.init 0" from "rand_select" defined above:
 * let permutation list = rand_select list (List.length list)
 * else : *)

let permutation list =
  let rec pop acc index list =
    match list with
    | [] -> raise (Unreachable "Uh, unreachable no?")
    | hd :: tl ->
        if index = 0 then
          hd,acc @ tl
        else
          pop (hd::acc) (index-1) tl
  in
  let rec aux acc list number =
    match list with
    | [] -> acc
    | hd :: tl ->
        if number = 0 then
          acc
        else
          let popped,remaining = pop [] (Random.int (List.length list)) list in
          aux (popped :: acc) remaining (number-1)
  in
  aux [] list (List.length list)

(* Problem 26 : Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List  *)

let extract k list =
  let rec concat acc list list2 =
    match list2 with
    | [] -> acc
    | hd :: tl -> concat ((list @ [hd] ) :: acc) list tl
  in
  let rec aux acc k list =
    match list with
    | [] -> acc
    | hd :: tl ->
        if List.length tl >= k then
          aux ((concat [] (hd::(slice tl 0 (k-2))) (slice tl (k-1) (List.length tl))) @ acc) k tl
        else
          acc
  in
  match k with
  | 0 -> []
  | 1 -> List.rev (concat [] [] list)
  | _ -> List.rev (aux [] (k-1) list)

(* Example's answer : way better and a lot more concise:
 * let rec extract k list =
 *   if k <= 0 then [[]]
 *   else match list with
 *        | [] -> []
 *        | h :: tl ->
 *           let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
 *           let without_h = extract k tl in
 *           with_h @ without_h;;
 * *)
