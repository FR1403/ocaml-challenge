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
assert (None <|> None <|> Some "dog" <|> None = Some "dog");;