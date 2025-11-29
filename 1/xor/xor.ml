let xor1 (n1: bool) (n2: bool) : bool =
  (n1 || n2) && not(n1 && n2)
;;

let xor2 (n1: bool) (n2: bool) : bool =
  if((n1 || n2) && not(n1 && n2)) then true
  else false
;;

let xor3 (n1: bool) (n2: bool) : bool =
  match (n1, n2) with
  | true, true -> false
  | true, false -> true
  | false, true -> true
  | false, false -> false
;;