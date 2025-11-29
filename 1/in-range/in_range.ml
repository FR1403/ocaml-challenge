let in_range (x: 'a) (a: 'a) (b: 'a) : bool =
  match x with
  | _ when (x >= a && x <= b) -> true
  | _ -> false 
;;

(* Test creati per vedere se il codice che ho scritto è corretto
(* Test con interi *)
assert (in_range 5 1 10 = true);;
assert (in_range 1 1 10 = true);;
assert (in_range 10 1 10 = true);;
assert (in_range 0 1 10 = false);;
assert (in_range 11 1 10 = false);;

(* Test con float *)
assert (in_range 5.5 1.0 10.0 = true);;
assert (in_range 1.0 1.0 10.0 = true);;
assert (in_range 10.0 1.0 10.0 = true);;
assert (in_range 0.5 1.0 10.0 = false);;
assert (in_range 10.5 1.0 10.0 = false);;

(* Test con char *)
assert (in_range 'c' 'a' 'z' = true);;
assert (in_range 'a' 'a' 'z' = true);;
assert (in_range 'z' 'a' 'z' = true);;
assert (in_range '0' 'a' 'z' = false);;

(* Test con string *)
assert (in_range "cat" "apple" "zebra" = true);;
assert (in_range "apple" "apple" "zebra" = true);;
assert (in_range "zebra" "apple" "zebra" = true);;
assert (in_range "Aaa" "apple" "zebra" = false);;

print_endline "✓ Tutti i test sono passati!";;*)
