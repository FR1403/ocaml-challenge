let sumrange (a: int) (b: int) : int =
  let rec sum (x: int) (y: int) : int = 
    if x <= y then x + sum (x+1) y
    else 0
  in 
  sum a b
;;

assert (sumrange 0 1 = 1);;
assert (sumrange 1 3 = 6);;
assert (sumrange 3 2 = 0);;