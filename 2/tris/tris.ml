let tris ((a: 'a), (b: 'a), (c: 'a), (d: 'a)) : bool =
  let check (a: 'a) (b: 'a) (c: 'a) : bool =
    if a = b && a = c then true
    else false 
  in
  if check a b c then true 
  else if check b c d then true
  else if check d c a then true 
  else if check d a b then true 
  else false 
;;

let hand = (Random.int (10) +1, Random.int (10) +1, Random.int (10) +1, Random.int(10) +1) ;;


(* 1. Caso: Tutti e 4 gli elementi sono uguali *)
assert (tris (5, 5, 5, 5) = true);;

(* 2. Casi: Esattamente 3 elementi uguali (ruotando la posizione del diverso) *)
assert (tris (1, 1, 1, 2) = true);; (* Il diverso è l'ultimo *)
assert (tris (1, 1, 2, 1) = true);; (* Il diverso è il terzo *)
assert (tris (1, 2, 1, 1) = true);; (* Il diverso è il secondo *)
assert (tris (2, 1, 1, 1) = true);; (* Il diverso è il primo *)
assert (tris (2, 1, 2, 1) = false);; (* Il diverso è il primo *)

(* 3. Casi: Solo 2 elementi uguali (Coppie) - Deve essere FALSE *)
assert (tris (1, 1, 2, 2) = false);;
assert (tris (1, 2, 1, 2) = false);;

(* 4. Casi: Tutti diversi - Deve essere FALSE *)
assert (tris (1, 2, 3, 4) = false);;

(* 5. Test con tipi diversi (visto che la tua funzione è polimorfica 'a) *)
assert (tris ("a", "a", "b", "a") = true);;
assert (tris (true, false, true, true) = true);;