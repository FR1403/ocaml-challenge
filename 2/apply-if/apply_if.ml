<<<<<<< HEAD
let apply_if (p: 'a -> bool) (f: 'a -> 'a) (x: 'a) : 'a =
  if p x then f x 
  else x 
;;

let is_even = fun x -> x mod 2 = 0;;
let is_odd = fun x -> not (is_even x);;
let double x = x * 2;;

assert(apply_if is_even double 4 = 8);;
assert(apply_if is_even double 5 = 5);;
assert(apply_if is_odd double 4 = 4);;
=======
let apply_if (p: 'a -> bool) (f: 'a -> 'a) (x: 'a) : 'a =
  let b = p x in
  match b with
  | true -> f x 
  | false -> x
;;

let is_even = fun x -> x mod 2 = 0;;
let is_odd = fun x -> not (is_even x);;
let double x = x * 2;;

assert(apply_if is_even double 4 = 8);;
assert(apply_if is_even double 5 = 5);;
assert(apply_if is_odd double 4 = 4);;
>>>>>>> 0c6e9e1 (altri es livello 2)
assert(apply_if is_odd double 5 = 10);;