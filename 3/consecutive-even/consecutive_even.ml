let consecutive_even (l: int list) : int =
  let is_even (n: int) : bool =
    if n mod 2 = 0 then true
    else false
  in
  let rec count (l: int list) (cons: int) (m: int) : int =
    match l with
    | h :: t -> if (is_even h) then count t (1+cons) (max (1+cons) m) else count t 0 m
    | [] -> m
  in

  count l 0 0 

;;

assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);; 
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;