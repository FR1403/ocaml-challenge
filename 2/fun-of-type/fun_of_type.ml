let f1 (n: int) : bool =
  if n >= 0 then true 
  else false
;;

let f2 (a: bool) : int =
  if true then 1 
  else 0
;;

let f3 (a: int) : (int * bool) =
  match a with
  | 0 -> 0,false
  | 1 -> 1, true
  | _  -> failwith "Valore non accettato"
;;

let f4 (a, b: int * bool) : int =
  if a >= 0 && b then a
  else 0
;;

let f5 (a: int) (f: int -> int) =
  
;;

let f6 (a: int) (p: int -> bool) = 
;;

let f7 (a: bool) (p: int -> bool) = 
;;


(* Test per f1 *)
assert (f1 5 = true);      (* numero positivo *)
assert (f1 0 = true);      (* zero *)
assert (f1 (-3) = false);  (* numero negativo *)

(* Test per f2 *)
assert (f2 true = 1);
assert (f2 false = 1);     (* f2 restituisce sempre 1 *)

(* Test per f3 *)
assert (f3 0 = (0, false));
assert (f3 1 = (1, true));
(* assert (f3 2) genera eccezione *)

(* Test per f4 *)
assert (f4 (3, true) = 3);
assert (f4 (3, false) = 0);
assert (f4 (-1, true) = 0);
assert (f4 (-1, false) = 0);
