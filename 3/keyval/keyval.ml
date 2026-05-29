let apply (l: ('a * 'b) list) (k: 'a) : 'b option =
  let rec find (l: ('a * 'b) list) (k: 'a) : 'b option =
    match l with
    | [] -> None
    | h :: t ->  match h with
                | k1, e1 when k1 = k -> Some e1
                | _ , _ -> find t k
  in 
  let find_comb (l: ('a * 'b) list) (k: 'a) : 'b option =
    let check (h: ('a * 'b)) : bool =
      match h with 
      | k1, e1 when k1 = k -> true
      | _, _ -> false
    in
    let give (e: ('a * 'b) list) : 'b option =
      match e with 
      | (a, b) :: t -> Some b
      | _ -> None
    in
    List.filter check l |> give 
  in

  find_comb l k 
  
;;

let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)];;
assert(apply f0 4 = Some 5);;
assert(apply f0 6 = None);;
assert(apply f0 2 = Some 3);;