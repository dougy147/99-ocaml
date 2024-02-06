(* Problem 31 : Determine whether a given intger number is prime *)
let is_prime n =
  if n = 2 then true else (* don't forget 2! *)
  if n = 1 || n mod 2 = 0 then false else
  let rec aux i n =
    if i * i > n then true else if n mod i = 0 then false else aux (i+2) n
  in
  aux 3 n

(* Problem 32 : Determine the greatest common divisor of two positive integer numbers *)

let gcd r1 r2 =
  let rec aux r1 r2 =
    let r0 = r1 mod r2 in
    if r0 = 0 then r2 else aux r2 r0
  in
  if r1 > r2 then aux r1 r2 else aux r2 r1

(* Problem 33 : Determine whether two positive integer numbers are coprime *)

let coprime n n' = gcd n n' = 1

(* Problem 34 : Calculate Euler's totient function φ(m) *)

(* Find out what the value of φ(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later). *)

let phi x =
  let rec p c x i =
    match i with
    | 0 | 1 -> 1 + c
    | _ -> if coprime x i then p (c+1) x (i-1) else p c x (i-1)
  in
  p 0 x (x-1)


(* Problem 35 : Determine the prime factors of a given positive integer *)

let factors n =
  let rec f acc n p =
    if p > n then acc else
    if is_prime p && n mod p = 0 then
        f ([p]@acc) (n / p) p
      else
        f acc n (p+1)
  in
  List.rev (f [] n 2)

(* Example's non-tail recursive solution:
let factors n =
  let rec f n p =
    if n = 1 then [] else
      if n mod p = 0 then p :: f (n/p) p else f n (p+1)
  in
  f n 2
*)

(* Problem 36 : Determine the prime factors of a given positive integer (2) *)
(* in a run-length form*)

(* first implementation *)
let factors n =
  let rec aux acc cur_p counter n p =
    if p > n then if counter = 0 then acc else ((cur_p,counter)::acc) else
    if is_prime p && n mod p = 0 then
      if p = cur_p then aux acc cur_p (counter+1) (n/p) p
      else if counter = 0 then aux acc p 1 (n/p) p
      else aux ((cur_p,counter)::acc) p 1 (n/p) p
    else aux acc cur_p counter n (p+1)
  in
  List.rev (aux [] 0 0 n 2)

(*
(* not tail recursive *)
let factors n =
  let rec aux p index n =
    if p > n then [] else
    if is_prime p && n mod p = 0 then
      if (n/p) mod p = 0 then aux p (index+1) (n/p) else (p,index) :: aux (p+1) 1 (n/p)
    else
      aux (p+1) 1 n
  in
  aux 2 1 n
*)

(* Problem 37 :  Calculate Euler's totient function φ(m) (improved). *)

(*
See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula:

φ(m) = (p1 - 1) × p1**(m1 - 1) × (p2 - 1) × p2**(m2 - 1) × (p3 - 1) × p3**(m3 - 1) × ⋯
*)

let phi_improved m =
  let rec integer_power a b = (* a to the power of b *)
    match b with
    | 0 -> 1 | 1 -> a | _ -> a * (integer_power a (b-1)) in
  let ( ** ) = integer_power in (* defining your own operators is wonderful *)
  let compute (p,m) = (p - 1) * (p ** (m - 1)) in
  let rec multiply_list = function
    | [] -> 1 | h :: t -> h * (multiply_list t) in
  multiply_list (List.map compute (factors m))


(* Problem 38 : Compare the two methods of calculating Euler's totient function. *)

let timeit f x =
  let start = Sys.time() in
  f x;
  Sys.time() -. start

(* Problem 39 : A list of prime numbers. *)

let all_primes infb supb =
  let rec aux acc infb supb =
    if infb = supb then List.rev acc
    else
      aux ((if is_prime infb then [infb] else []) @ acc) (infb+1) supb
  in
  aux [] infb supb

(* Problem 40 : Goldbach's conjecture *)

(* Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer. *)

let goldbach x =
  if x mod 2 != 0 then failwith "Not an even number" else
  let rec aux p x =
    if (is_prime p) && (is_prime (x-p)) then (p,x-p)
    else aux (p+1) x
  in
  aux 2 x


(* Problem 41 : A list of Goldbach compositions. *)

let goldbach_list infb supb =
  let rec aux acc i s =
    if i > s then List.rev acc else
    if i mod 2 = 0 then aux ((i,goldbach i)::acc) (i+2) s
    else aux acc (i+1) s
  in
  aux [] infb supb

(* without List.filter built-in *)
let goldbach_limit infb supb maxval =
  let rec aux acc m = function
    | [] -> List.rev acc
    | (n, (x,y) ) as hd :: tl when x >= m && y >= m -> aux (hd :: acc) m tl
    | _ :: tl -> aux acc m tl
  in
  aux [] maxval (goldbach_list infb supb)

(* with List.filter *)
let goldbach_limit infb supb maxval =
  List.filter (fun (n, (x,y)) -> x >= maxval && y >= maxval ) (goldbach_list infb supb)

