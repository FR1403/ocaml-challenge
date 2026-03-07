type suit = S | H | D | C ;;
type card = Card of int * suit ;;

Random.self_init() ;;

let rndHand () : card * card * card * card * card = 
  let create_card () : card = 
    let suit = 
      match (Random.int (4) +1) with
      | 1 -> S
      | 2 -> H
      | 3 -> D 
      | 4 -> C
      | _ -> failwith "Boh"
    in
    Card (Random.int 13 +1, suit)
  in
  (create_card (), create_card (), create_card (), create_card (), create_card())
;;

(* let create_hand ((c1: card), (c2:card), (c3:card), (c4:card), (c5:card)) : card * card * card * card * card =
  rndHand() 
;; *)

let same_value (c1: card) (c2:card) (c3: card) (c4:card) : bool =
  match c1, c2, c3, c4 with
  | Card (v1, s1), Card (v2, s2), Card (v3, s3), Card (v4, s4) when v1 = v2 && v2 = v3 && v3 = v4 -> true
  | _ , _ , _ , _ -> false
;;

let same_suit (c1: card) (c2:card) (c3:card) (c4:card): bool =
  match c1, c2, c3, c4 with
  | Card (v1, s1), Card (v2, s2), Card (v3,s3), Card(v4,s4) when 
          (s1 <> s2) && (s2 <> s3) && (s3 <> s4) && (s1 <> s3) && (s1 <> s4) &&
          (s2 <> s4) -> false
  | _ , _ , _, _-> true
;;

let poker ((c1: card), (c2:card), (c3:card), (c4:card), (c5:card)) : bool =
  let check (c1: card) (c2:card) (c3:card) (c4:card) : bool =
    if same_value c1 c2 c3 c4  && not (same_suit c1 c2 c3 c4) then true
    else false
  in
  if check c1 c2 c3 c4 || check c2 c3 c4 c5 || check c3 c4 c5 c1 
    || check c1 c2 c4 c5 || check c1 c2 c3 c5 then true 
  else false 
;;

let prova (p: (card * card * card * card * card) -> bool) (r: unit -> (card * card * card * card * card)) : bool =
  if p (r ()) then true 
  else false 
;;


(* --- TEST 1: Poker Servito (Quattro assi e un sette) --- *)
let mano_poker_assi = (
  Card(1, S), Card(1, H), Card(1, D), Card(1, C), Card(7, S)
);;
assert (poker mano_poker_assi = true);;

(* --- TEST 2: Poker con l'ultima carta diversa (Quattro Re e un Due) --- *)
let mano_poker_re = (
  Card(13, S), Card(13, H), Card(13, D), Card(13, C), Card(2, S)
);;
assert (poker mano_poker_re = true);;

(* --- TEST 3: Poker "centrale" (Il diverso è in mezzo) --- *)
let mano_poker_misto = (
  Card(10, S), Card(5, H), Card(10, H), Card(10, D), Card(10, C)
);;
assert (poker mano_poker_misto = true);;

(* --- TEST 4: Tris (Tre uguali NON è un poker) --- *)
let mano_tris = (
  Card(1, S), Card(1, H), Card(1, D), Card(2, C), Card(3, S)
);;
assert (poker mano_tris = false);;

(* --- TEST 5: Doppia Coppia (NON è un poker) --- *)
let mano_doppia_coppia = (
  Card(10, S), Card(10, H), Card(5, D), Card(5, C), Card(2, S)
);;
assert (poker mano_doppia_coppia = false);;

(* --- TEST 6: Caso "Imbroglio" (Quattro valori uguali ma STESSO SEME) --- *)
(* In un mazzo vero è impossibile, ma testa se la tua same_suit funziona *)
let mano_falsa = (
  Card(1, S), Card(1, S), Card(1, S), Card(1, S), Card(5, D)
);;
assert (poker mano_falsa = false);;

(* let rec simula tentativi =
  let m = rndHand () in
  if poker m then 
    Printf.printf "Poker trovato al tentativo numero %d!\n" tentativi
  else 
    simula (tentativi + 1)
;;
Attenzione: potrebbe metterci qualche secondo a finire! *)