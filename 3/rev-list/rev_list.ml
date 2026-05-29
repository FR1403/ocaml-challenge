let rev (l: 'a list) : 'a list =
  let rec reverse (s: 'a list) : 'a list =
    match s with 
    | h :: [] -> [h]
    | h :: t -> (reverse t) @ [h]
    | [] -> []
  in
  match l with 
  | h :: t -> reverse (h :: t) 
  | [] -> []
;;

