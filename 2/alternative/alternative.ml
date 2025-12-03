<<<<<<< HEAD
let (<|>) (arg1: 'a option) (arg2: 'a option) : 'a option =
  match arg1,arg2 with
  | None, None -> None
  | Some arg1, _ -> Some arg1
  | None, Some arg2 -> Some arg2
;;

assert (None <|> Some false = Some false);;
assert (Some true <|> None <|> Some false = Some true);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
=======
let (<|>) (x: 'a option) (y: 'a option) : 'a option =
  match (x, y) with
  | None , None -> None 
  | Some n , _ -> Some n
  | _ , Some m -> Some m
;;

assert (None <|> Some false = Some false);;
assert (Some true <|> None <|> Some false = Some true);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
>>>>>>> 0c6e9e1 (altri es livello 2)
assert (None <|> None <|> Some "dog" <|> None = Some "dog");;