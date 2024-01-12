(* Problem 31 : Determine whether a given intger number is prime *)
let is_prime n =
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


