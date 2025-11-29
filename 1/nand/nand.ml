let nand1 (n1: bool) (n2: bool) : bool =
  not (n1 && n2);;

let nand2 (n1: bool) (n2: bool) : bool =
  if(n1 && n2) then false
  else true;;

let nand3 (n1: bool) (n2: bool) : bool =
  match (n1, n2) with 
  | true, true -> false
  | true, false -> true
  | false, _ -> true ;;
  
