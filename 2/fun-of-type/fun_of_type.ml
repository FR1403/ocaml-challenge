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

let f5 (a: int) : int -> int = 
  fun a -> a + 1
;;

let f6 (a: int) : int -> bool = 
  fun a -> if a>= 0 then true else false
;;

let f7 (a: bool) : int -> bool =
  fun x -> a
;;

let f8 (b: bool) : bool -> int =
  fun b -> if b then 1 else 0
;;

let f9 (b: bool) : int -> int =
  fun x -> if b then 1 else 0
;;

(* let double (a: int) : int = 
  a*2
;; *)

let f10 (f: int -> int) : int =
  f 2
;;

let f11 (f: int -> bool) : int =
  if(f 2) then 1 else 0
;;

let f12 (f: bool -> int) : int =
  f true
;;

let f13 (f: int -> bool) : bool =
  f 3
;;

let f14 (f: bool -> bool) : int =
  if f false then 1 else 0
;;

let f15 (a: int)(b,c: int * int) : int =
  match a with
  | a when a >= 0 -> b 
  | _ -> c
;;

let f16 (n: int) : int -> (int -> int) =
  fun n -> fun x ->  n + 1
;;

let f17 (f: int -> int) : int -> int =
  fun x -> f 3 
;;

let f18 (f: (int -> int) -> int ) : int = 
  f (fun x -> 3)
;;

let f19 (f: int -> int) : bool -> bool =
  if (f 0) = 0 then fun x -> x && true else fun y -> y && false
;;

let f20 (f: int -> bool) : bool -> int =
  if f 3 then fun x -> 3 else fun y -> 4
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
