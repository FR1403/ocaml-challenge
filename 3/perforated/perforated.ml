let is_perforated (l: int list) : bool =
  let rec check (s: int list) : bool =
    match s with
    | e1 :: e2 :: [] -> if abs(e1-e2) = 1 then false else true
    | e1 :: e2 :: t -> if abs(e1-e2) = 1 then false else check (e2::t)
    | h :: [] -> true
    | [] -> true
  in
  check l
;;

assert(is_perforated []);;
assert(is_perforated [1]);;
assert(is_perforated [1;2] = false);;
assert(is_perforated [1;3]);;
assert(is_perforated [1;5;2]);;
assert(is_perforated [1;3;2] = false);;
assert(is_perforated [1;4;2;0]);;
assert(is_perforated [1;3;2;0] = false);;
assert(is_perforated [1;3;5;2;4;7;3;1]);;