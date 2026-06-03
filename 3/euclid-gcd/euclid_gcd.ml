let gcd (a: int) (b: int) : int =
  let rec aux_gcd (a: int) (b: int) : int =
    if b = 0 then a 
    else aux_gcd b (a mod b)
  in
  aux_gcd a b
;;