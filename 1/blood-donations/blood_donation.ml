type blood_group = A | B | AB | O

let check_groups (b1: blood_group) (b2: blood_group) : bool =
  match b1 with
  | A -> (match b2 with
        | A | AB -> true
        | _ -> false)
  | B -> (match b2 with
        | B | AB -> true 
        | _ -> false)
  | AB -> (match b2 with
        | AB -> true
        | _ -> false)
  | O -> true
;;

assert (check_groups O AB = true);;
assert (check_groups A B = false);;
assert (check_groups AB AB = true);;