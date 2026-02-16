let sub (x: int) (y: int) = x - y;;

let flip (f: 'a -> 'b -> 'c) : 'b -> 'a -> 'c =
  fun x y -> f y x  
;;

let flipped_sub = flip sub;;

assert (flipped_sub 3 10 = 7);;
assert (flipped_sub 10 3 = -7);;