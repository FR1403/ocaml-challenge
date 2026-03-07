let is_posfrac (a,b : int * int) : bool =
  if b = 0 then failwith "Divisione per zero non possibile"
  else 
    match (a*b) with
    | v when v >= 0 -> true
    | _ -> false 
;; 

(*
0 se entrambe le frazioni sono equivalenti
1 se la prima frazione è più grande della seconda
-1 altrimenti*)
let compare_posfrac (a,b : int * int) (c,d : int * int) : int =
  if is_posfrac (a,b) && is_posfrac (c,d) then 
    match (a*d) , (c*b) with
  | fst , snd when fst > snd -> 1
  | fst , snd when fst = snd -> 0
  | _ , _ -> -1
  else failwith "Una delle frazioni è negativa"
;;

let compare_frac (a,b : int * int) (c,d : int * int) : int = 
  match is_posfrac (a,b), is_posfrac (c,d) with
  | true, true -> compare_posfrac (a,b) (c,d)
  | true, false -> 1
  | false, true -> -1
  | false, false -> 
      if (a < 0) && (c < 0) then 
        match (a*d), (c*b) with
        | fst , snd when fst > snd -> 1
        | fst , snd when fst = snd -> 0
        | _ , _  -> -1
      else 
        if (a > 0) && (c > 0) then
          match (-a*(-d)), (-c*(-b)) with
          | fst , snd when fst > snd -> 1
          | fst , snd when fst = snd -> 0
          | _ , _  -> -1
        else if (a < 0) && (c > 0) then 
          match (a*(-d)), (-c*b) with
          | fst , snd when fst > snd -> 1
          | fst , snd when fst = snd -> 0
          | _ , _  -> -1
        else
          match (-a*d), (c*(-b)) with
          | fst , snd when fst > snd -> 1
          | fst , snd when fst = snd -> 0
          | _ , _  -> -1
;;

assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;



(* 1. Entrambe Positive *)
assert (compare_frac (1, 2) (1, 3) = 1);;    (* 1/2 > 1/3 *)
assert (compare_frac (1, 4) (1, 2) = -1);;   (* 1/4 < 1/2 *)
assert (compare_frac (2, 4) (1, 2) = 0);;    (* Equivalenti *)

(* 2. Segni Opposti (Vittoria facile della positiva) *)
assert (compare_frac (1, 2) (-1, 2) = 1);;   (* Positiva > Negativa *)
assert (compare_frac (-3, 4) (1, 10) = -1);; (* Negativa < Positiva *)

(* 3. Entrambe Negative (Numeratori e denominatori vari) *)
(* Caso: a < 0 e c < 0 *)
assert (compare_frac (-1, 2) (-2, 3) = 1);;  (* -1/2 (-0.5) > -2/3 (-0.66) *)
assert (compare_frac (-3, 4) (-1, 4) = -1);; (* -3/4 < -1/4 *)

(* Caso: a > 0 e c > 0 (denominatori negativi) *)
assert (compare_frac (1, -2) (1, -3) = -1);; (* -1/2 < -1/3 *)

(* Caso: Incrociati (es. -1/2 e 1/-2) - Equivalenza *)
assert (compare_frac (-1, 2) (1, -2) = 0);;  (* Entrambe valgono -0.5 *)

(* 4. Lo zero (visto come frazione positiva o negativa nel tuo codice) *)
(* Nota: il tuo is_posfrac ritorna false per lo zero (a*b = 0) *)
assert (compare_frac (0, 1) (1, 2) = -1);;   (* 0 < 1/2 *)
assert (compare_frac (0, 1) (-1, 2) = 1);;   (* 0 > -1/2 *)