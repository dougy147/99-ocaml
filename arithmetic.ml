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
