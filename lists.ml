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
let length xs =
  let rec l acc xs =
    match xs with
    | [] -> acc
    | _ :: t -> l (acc + 1) t
  in
  l 0 xs

(* Should do the same as: *)
(* let length list =
  let rec l acc = function (* function is right-associative *)
    | [] -> acc
    | _ :: t -> l (acc + 1) t
  in
  l 0 list
*)

(* Problem 05 : Reverse a List *)

let rev xs =
  let rec r reversed xs =
    match xs with
    | [] -> reversed
    | h :: t -> r (h :: reversed) t
  in
  r [] xs

(* Problem 06 : Palindrome *)

let palindrome xs =
  xs = rev xs

(* Problem 07 : Flatten a nested list structure *)

(* we create a new type called 'node' that can be 'One' or 'Many' *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten xs =
  let rec f acc xs =
    match xs with
    | One x :: rest -> f (x :: acc) rest
    | Many x :: rest -> f acc (x @ rest)
    | _ -> rev acc
  in
  f [] xs

(* Problem 08 : Eliminate Consecutive Duplicates *)
let compress xs =
  let rec c acc xs =
    match xs with
    | [] -> acc
    | [x] -> x :: acc
    | x :: y :: rest ->
        if x = y then
          c acc (y :: rest)
        else
          c (x :: acc) (y :: rest)
  in
  rev (c [] xs)

(*  (* From the official example *)
let rec compress2 xs =
  match xs with
  | first :: (second :: _ as t) ->
      if first = second then
        compress2 t
      else
        first :: compress2 t
  | smaller -> smaller (*smaller means any value that hasn't been matched in the previous ones. It is an identifier*)
*)

(* Problem 09 : Pack consecutive duplicates into subxss*)

let pack xs =
  let rec p acc acc2 xs =
    match xs with
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
  p [] [] xs

(* Problem 10 : Run-length encoding of a List *)

let encode xs =
  let rec e acc xs =
    match xs with
    | [] | [] :: _ -> acc
    | (h::r) :: rest -> e ((length (h::r), h)::acc) rest
  in
  e [] (rev (pack xs))

(* Problem 11 : Modified run-length encoding*)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode xs =
  let rec modif acc xs =
    match xs with
    | [] -> acc
    | (x,y) :: rest when x > 1 -> modif ((Many (x,y))::acc) rest
    | (x,y) :: rest -> modif ((One y)::acc) rest
  in
  let rec e acc xs =
    match xs with
    | [] | [] :: _ -> rev (modif [] acc)
    | (h::r) :: rest -> e ((length (h::r), h)::acc) rest
  in
  e [] (rev (pack xs))

(* Problem 12 : Decode a run-length encoded list *)
(* decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];; *)

let decode xs =
  let rec unwrap acc index symbol =
    match index with
    | 1 -> (symbol::acc)
    | _ -> unwrap (symbol::acc) (index-1) symbol
  in
  let rec d acc xs =
    match xs with
    | [] -> rev acc
    | Many (x,y) :: rest -> d ((unwrap [] x y)@acc) rest
    | One x :: rest -> d (x::acc) rest
  in
  d [] xs

(* Problem 13 : Run-length encoding of a list (direct solution) *)
(* i.e. not storing the list of duplicates but immediately counting. *)

let encode xs =
  let count index symbol = if index = 1 then (One symbol) else (Many (index,symbol)) in
  (*let rec label acc xs = (*what first came to mind. non-optimal, right?*)
    match xs with
    | [] -> acc
    | (l,x)::tl when l > 1 -> label ((Many(l,x))::acc) tl
    | (l,x)::tl -> label ((One x)::acc) tl
  in*)
  let rec e acc index xs =
    match xs with
    | [] -> acc
    | hd :: [] -> (count index hd::acc)
    | first::second::tail ->
        if first = second then
          e acc (index+1) (second::tail)
        else
          e ((count index first)::acc) 1 (second::tail)
  in
  rev (e [] 1 xs)


(* Problem 14 : Duplicate the elements of a list *)

let duplicate xs =
  let rec dup acc = function
    | [] -> acc
    | hd :: tl -> dup (hd::hd::acc) tl
  in
  List.rev (dup [] xs)

(* non tail recursive solution *)
(*let rec duplicate = function
  | [] -> []
  | hd :: tl -> hd::hd::(duplicate tl)*)

(* Problem 15 : Replicate the elements of a list a given number of times *)

let replicate xs repeat=
  let rec mult acc index item =
    if index = 0 then acc else mult (item::acc) (index-1) item
    (* match index with
    | 0 -> acc
    | _ -> mult (item::acc) (index-1) item *)
  in
  let rec r xs repeat =
    match xs with
    | [] -> []
    | hd :: tl -> (mult [] repeat hd) @ (r tl repeat)
  in
  r xs repeat

(* Problem 16 : Drop every N'th element from a xs *)

let drop xs n =
  let rec d original_n n = function
    | [] -> []
    | head :: tail -> if n = 1 then d original_n original_n tail else head::(d original_n (n-1) tail)
  in
  d n n xs
(* Original solution is better with index i: if i = n ... *)


(* Problem 17 : Split a List Into Two Parts; The Length of the First Part Is Given *)

let split xs length =
  let rec s acc xs l =
    match xs with
    | [] -> (List.rev acc,[])
    | hd :: tl as initxs -> if l = 0 then (List.rev acc,initxs) else s (hd::acc) tl (l-1)
  in
  s [] xs length

(* Problem 18 : Extract a slice from a xs *)

let slice xs i k =
  let rec s acc xs i range =
    match i,range with
    | 0,0 -> List.rev (List.hd xs :: acc)
    | 0,_ -> if List.tl xs = [] then
               List.rev (List.hd xs :: acc)
             else
               s (List.hd xs::acc) (List.tl xs) i (range-1)
    | _ -> s acc (List.tl xs) (i-1) range
  in
  if k < i || i >= length xs then [] else s [] xs i (k-i)

(* Problem 19 : Rotate a xs N places to the left *)

let rotate xs n =
  let rec r acc n xs =
    if n = 0 then xs @ (List.rev acc) else r (List.hd xs::acc) (n-1) (List.tl xs)
  in
  if List.length xs = 0 then [] else r [] (n mod (List.length xs)) xs

(* Problem 20 : Remove the K'th element from a xs *)

let remove_at k xs =
  let rec r_at acc k = function
    | [] -> List.rev acc
    | hd :: tl -> if k = 0 then (List.rev acc) @ tl else r_at (hd::acc) (k-1) tl
  in
  r_at [] k xs
(* More performant than the example's solution *)

(* Problem 21 : Insert an Element at a Given Position Into a List *)

let insert_at element index xs =
  let rec aux acc elem index = function (* starting to use the name 'aux' that seems common use? *)
    | [] -> (List.rev acc) @ [elem]
    | l when index = 0 -> (List.rev acc) @ [elem] @ l
    | hd :: tl -> aux (hd :: acc) elem (index-1) tl
  in
  aux [] element index xs

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
let rand_select xs number =
  Random.init 0;
  let rec nth index = function
    | [] -> raise (Unreachable "Uh, unreachable no?")
    | hd :: tl -> if index = 0 then [hd] else nth (index-1) tl
  in
  let rec aux acc xs n =
    if n = 0 then acc else aux ((nth (Random.int (List.length xs)) xs) @ acc) xs (n-1)
  in
  aux [] xs number

(* without replacement *)
let rand_select xs number =
  Random.init 0;
  let rec pop acc index xs =
    match xs with
    | [] -> raise (Unreachable "Uh, unreachable no?")
    | hd :: tl ->
        if index = 0 then
          hd,acc @ tl
        else
          pop (hd::acc) (index-1) tl
  in
  let rec aux acc xs number =
    match xs with
    | [] -> acc
    | hd :: tl ->
        if number = 0 then
          acc
        else
          let popped,remaining = pop [] (Random.int (List.length xs)) xs in
          aux (popped :: acc) remaining (number-1)
  in
  aux [] xs number

(* Problem 24 : Draw N Different Random Numbers From the Set 1..M *)
(* There seems to be a trade-off between those two functions *)

(* "Good" for small n but large m *)
let lotto_select n m =
  let rec is_in_acc xs number =
    match xs with
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
 * let permutation xs = rand_select xs (List.length xs)
 * else : *)

let permutation xs =
  let rec pop acc index xs =
    match xs with
    | [] -> raise (Unreachable "Uh, unreachable no?")
    | hd :: tl ->
        if index = 0 then
          hd,acc @ tl
        else
          pop (hd::acc) (index-1) tl
  in
  let rec aux acc xs number =
    match xs with
    | [] -> acc
    | hd :: tl ->
        if number = 0 then
          acc
        else
          let popped,remaining = pop [] (Random.int (List.length xs)) xs in
          aux (popped :: acc) remaining (number-1)
  in
  aux [] xs (List.length xs)

(* Problem 26 : Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List  *)
(* Copy of the example's solution to learn about List.map. *)
let rec extract k xs =
  if k <= 0 then [[]] else
  match xs with
  | [] -> []
  | hd :: tl ->
      let prepend_head = List.map (fun x -> hd :: x) (extract (k-1) tl) in
      let without_head = extract k tl in
      prepend_head @ without_head

(* Here is my first take to this problem, it is incorrect but maybe not that far of solving. To check again.*)
(* e.g. DOES NOT work with extract 3 ["a";"b";"c";"d"] -> missing ["a";"c";"d"]

 * let extract k xs =
 *   let rec concat acc xs xs2 =
 *     match xs2 with
 *     | [] -> acc
 *     | hd :: tl -> concat ((xs @ [hd] ) :: acc) xs tl
 *   in
 *   let rec aux acc k xs =
 *     match xs with
 *     | [] -> acc
 *     | hd :: tl ->
 *         if List.length tl >= k then
 *           aux ((concat [] (hd::(slice tl 0 (k-2))) (slice tl (k-1) (List.length tl))) @ acc) k tl
 *         else
 *           acc
 *   in
 *   match k with
 *   | 0 -> []
 *   | 1 -> List.rev (concat [] [] xs)
 *   | _ -> List.rev (aux [] (k-1) xs)
 *)

(* Problem 27 : Group the elements of a set into disjoint subsets *)
(* Hard one for me. Does not take into account permutations when giving [1;1;1] as xs_numbers.
  i.e. 'group ["a";"b";"c"] [1;1;1]' returns [["a"];["b"];["c"]]; [["b"];["c"];["a"]]... even if are the same.
 *)

let group group_xs numbers_xs =
  let rec disjonction acc xs1 xs2 =
    match xs1 with
    | [] -> List.rev acc @ xs2
    | h1 :: t1 as l1 -> match xs2 with
        | [] -> disjonction (h1::acc) t1 xs2
        | h2 :: t2 -> if h1 = h2 then disjonction acc t1 t2 else disjonction (h2::acc) l1 t2 in

  let rec aux acc g n =
    match n with
    | [] -> [[]]
    | [k] -> List.map (fun x -> acc @ [x]) (extract k g)
    | k :: ktl ->
        let prefixes = extract k g in
        prefixes
        |> List.map (fun pref ->
            (aux (acc@[pref]) (disjonction [] pref g) ktl)
            )
        |> List.flatten
  in
  aux [] group_xs numbers_xs


(* Problem 28 :  Sorting a xs of xss according to length of subxss *)
(* Given the example's solution, we "might not be allowed to use the built-in List.sort" function. At the moment, I don't mind (learning built-ins is also learning ;p). *)

let length_sort xs = List.sort (fun x y -> compare (List.length x) (List.length y)) xs

let frequency_sort xs =
  let original = xs in
  let rec count counter item xs =
    match xs with
    | [] -> counter
    | hd :: tl -> if List.length hd = List.length item then count (counter+1) item tl else count counter item tl
  in
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | hd :: tl ->
        let c = count 0 hd original in
        aux ([(c,hd)] @ acc) tl
  in
  let sort_tuples tuples = List.sort (fun (x,xs) (y,ys) -> compare x y) tuples in
  let rec extract_from_tuples acc tuples =
    match tuples with
    | [] -> List.rev acc
    | (x,y) :: tl -> extract_from_tuples ([y] @ acc) tl
  in
  extract_from_tuples [] (sort_tuples (aux [] xs))
