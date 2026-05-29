type grade = Val of int | CumLaude ;;

let is_valid (g: grade) : bool =
  if (g >= Val 18 && g <= Val 30) || g = CumLaude then true 
  else false 
;;

let int_of_grade (g: grade) : int =
  if is_valid g then 
    match g with
    | CumLaude -> 32
    | Val n -> n
  else failwith "Is not an accettable value" 
;;

let avg (l: grade list) : float =
  let rec check (s: grade list) : bool =
    match s with 
    | [] -> failwith "Empty list"
    | h :: [] when is_valid h -> true
    | h :: t -> if is_valid h then check t else failwith "There is an invalid grade" 
  in
  let rec sum (l: grade list) : int =
    match l with 
    | [] -> 0
    | h :: [] -> int_of_grade h
    | h :: t -> int_of_grade h + sum t
  in
  if check l then (float_of_int(sum l) /. float_of_int(List.length l)) 
  else failwith "Not valid list"
;;

let combinator_sum (l: int list) : int =
  List.fold_right (+) l 0
;;

let avg_norec (l: grade list) : float =
  (List.filter is_valid l) |> avg
;;

(* --- ASSERTS --- *)

(* Asserts per is_valid *)
assert (is_valid (Val 18) = true);;
assert (is_valid (Val 30) = true);;
assert (is_valid (Val 25) = true);;
assert (is_valid (Val 17) = false);;
assert (is_valid (Val 31) = false);;
assert (is_valid CumLaude = true);;

(* Asserts per int_of_grade *)
assert (int_of_grade (Val 18) = 18);;
assert (int_of_grade (Val 30) = 30);;
assert (int_of_grade CumLaude = 32);;
assert (try int_of_grade (Val 17) |> fun _ -> false with _ -> true);;

(* Asserts per avg *)
assert (avg [Val 24; Val 26; Val 28] = 26.0);;
assert (avg [CumLaude; Val 18; Val 22] = 24.0);;
assert (try avg [Val 18; Val 17] |> fun _ -> false with _ -> true);;
assert (try avg [] |> fun _ -> false with _ -> true);;

(* Asserts per avg_norec *)
assert (avg_norec [Val 24; Val 17; Val 26; Val 28; Val 31] = 26.0);;
assert (avg_norec [CumLaude; Val 18; Val 15; Val 22] = 24.0);;
assert (try avg_norec [Val 17; Val 15] |> fun _ -> false with _ -> true);;