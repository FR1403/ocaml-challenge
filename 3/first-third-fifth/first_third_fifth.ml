let first_third_fifth (l: 'a list) : ('a * 'a * 'a) option =
  if List.length l < 5 then None
  else match l with
      | e1 :: e2 :: e3 :: e4 :: e5 :: t -> Some (e1, e3, e5)
      | _ -> None
;;

assert(first_third_fifth ["cat"; "dog"] = None);;
assert(first_third_fifth [1; 2; 3; 4; 5; 6] = Some (1, 3, 5))