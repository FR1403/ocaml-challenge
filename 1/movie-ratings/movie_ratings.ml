let movie_rating (n1: int) (n2: int) (n3: int) : string =
  if (n1 < 1 || n2 < 1 || n3 < 1) || (n1 > 5 || n2 > 5 || n3 > 5) then
    failwith "Il valore inserito non è valido"
  else
    (match (n1, n2, n3) with
    | 5, 5, 5 -> "Masterpiece"
    | (5, 5, 4) | (5, 4, 5) | (4, 5, 5) -> "Highly Recommended"
    | _, _, _ when ((n1 >= 4 && n2 >= 4 && n3 >= 3) || 
                    (n1 >= 3 && n2 >= 4 && n3 >= 4) ||
                    (n1 >= 4 && n2 >= 3 && n3 >= 4))  -> "Recommended"
    | _, _, _ -> "Mixed Reviews")
;;