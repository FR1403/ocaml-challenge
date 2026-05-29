type nat = Z | S of nat
let rec int_of_nat (n: nat) : int =
    match n with 
    | S n -> 1 + int_of_nat n
    | Z -> 0
;;

let rec nat_of_int (n: int) : nat =
  if n >= 1 then S (nat_of_int (n-1))
  else Z 
  
;;

let iseven (n: nat) : bool =
  if (int_of_nat n) mod 2 = 0 then true 
  else false 
;;

let halve (n: nat) : nat = 
  nat_of_int ((int_of_nat n)/2) 
;;

let add (n1: nat) (n2: nat) : nat =
  nat_of_int((int_of_nat n1) + (int_of_nat n2))
;;

let mul (n1: nat) (n2: nat) : nat =
  nat_of_int((int_of_nat n1) * (int_of_nat n2))
;;

let equals (n1: nat) (n2: nat) : bool =
  if n1 = n2 then true
  else false
;;

let leq (n1: nat) (n2: nat) : bool =
  if n1 <= n2 then true 
  else false
;;

