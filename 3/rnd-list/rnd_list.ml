let rnd_list (n: int) (b: int) : int list =
  let rec create (l: int list) (n: int) (b: int) : int list =
    if n >= 1 then 
      match l with
      | h :: [] -> h :: create [(Random.int b) + 1] (n-1) b 
      | h :: t -> h :: create [(Random.int b) + 1] (n-1) b 
      | [] -> create [(Random.int b) + 1] (n-1) b
    else l
  in

  create [] n b

;;