(* https://ocaml.org/exercises *)
(* https://v2.ocaml.org/learn/tutorials/99problems.html *)

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
