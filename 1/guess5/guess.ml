let guess5 (n: int) : bool * int =
  if n < 1 || n > 5 then 
    failwith "Numero fuori dal range"
  else
    let r = Random.int(5) + 1 in
    match (n, r) with
    | _ , _ when (n = r) -> true , r
    | _ , _ -> false , r
  ;; 