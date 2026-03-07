(*implementa una funzione che restituisca 1
se x appartiene ad A e restituisca 0 se x non 
appartiene ad A*)
let f1 (x: int) : int = 
  0
;;

let f2 (x: int) : int = 
  if x < 0 then failwith "undefined"
  else 
    if x >= 0 && x <=2 then 1 else 0
;;

let f3 (x: int) : int =
  if x < 0 then failwith "undefined" else
  if x > 0 && x < 100 then 1 else 0
;;

let f4 (x: int) : int =
  if x < 0 then failwith "undefined" else 0
;;

let f5 (x: int) : int =
  if x < 0 then failwith "undefined" else 1
;;

(*se x = y + y vuol dire che x è il doppio di y
quindi x deve essere pari*)
let f6 (x: int) : int =
  if x < 0 then failwith "undefined"
  else if x mod 2 = 0 then 1 else 0
;;

let f7 (x: int) : int =
  if x < 0 then failwith "undefined"
  else 1
;;

let f8 (x: int) : int =
  if x < 0 then failwith "undefined"
  else 1
;;

let f9 (x: int) : int =
  if x < 1 then failwith "undefined"
  else 1
;;

let f10 (x: int) : int =
  if x < 0 then failwith "undefined"
  else if x >= 4 && x <= 19 then 1 else 0
;;

let f11 (x: int) : int =
  if x < 0 then failwith "undefined"
  else if x < 50 && (x mod 2 = 0) then 1
  else 0
;;

let f12 (z: int) : int =
  if z < 0 then failwith "undefined" 
  else if (z mod 2 = 0) && (z > 0) && (z < 50) then 1 else 0
;;

(* let f13 (z: int) : int =
*)

let f14 (x: int) : int =
  if x < 0 then failwith "undefined" 
  else if x >= 1 && x <= 2 then 1 else 0
;;

(*x deve avere come divisori solo 1 e se stesso
(numeri primi)*)
let f15 (x: int) : int =
  let rec is_prime (n: int)(d: int) : bool =
    if n mod 2 = 0 && n = 2 then true 
    else if n mod 2 = 0 && n <> 2 then false 
    else if n mod d = 0 && n = d then is_prime n (d-2)
    else if n mod d = 0 && n <> d && d>1 then false
    else if (n mod d <> 0) && d > 1 then is_prime n (d-2)
    else true
  in  

  if x < 0 then failwith "undefined"
  else if is_prime x x then 1 else 0
;;