let max (a: int option) (b: int option) : int option =
  match (a, b) with 
  | None, None -> None
  | None, _ -> b
  | _ , None -> a
  | _ , _ when (a >= b) -> a 
  | _ , _ -> b
;;


let best_offer (n1: int option) (n2: int option) (n3: int option) : int option =
  if (n1 = None && n2 = None && n3 = None) then None
  else 
  max (max n1 n2) n3
;;

