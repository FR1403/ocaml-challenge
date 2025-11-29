let incr_opt (n: int option) : int option =
  match n with
  | Some n -> Some (n + 1)
  | None -> None
;;