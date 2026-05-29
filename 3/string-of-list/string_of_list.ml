let string_of_list (l: int list) : string =
  let rec string (s: int list) : string =
    match s with 
    | h :: [] -> string_of_int(h) ^ "]"
    | h :: t -> string_of_int(h) ^ ";" ^ string t 
    | [] ->  "]"
  in
  "[" ^ string l
;;