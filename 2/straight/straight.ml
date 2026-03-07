type suit = S | H | D | C ;;
type card = Card of int * suit ;;

let rec max_of (v1:int) (v2:int) (v3:int) (v4:int) (v5:int) : int =
  if v1 > v2 && v1 > v3 && v1 > v4 && v1 > v5 then v1 
  else max_of v2 v3 v4 v5 v1
;;

let rec ordered (l: int list) : bool =
  let lista_ordinata = List.sort compare l in
  match lista_ordinata with
  | [] -> false
  | h :: [] -> true
  | h :: v :: t -> if h = (v-1) then ordered (v :: t) else false
;;

let is_straight (c1:card) (c2:card) (c3:card) (c4:card) (c5:card) : bool =
  match c1, c2, c3, c4, c5 with 
  | Card (v1, s1), Card(v2, s2), Card (v3, s3), Card (v4, s4),Card (v5, s5) when ordered [v1; v2; v3; v4; v5] -> true 
  | _ , _ , _ , _ ,_ -> false
;;

let straight ((c1:card), (c2:card), (c3:card), (c4:card), (c5:card)) : bool =
  match c1, c2, c3, c4, c5 with 
  | _ , _ , _ , _ , _ when is_straight c1 c2 c3 c4 c5 -> true 
  | _ , _ , _ , _ , _ -> false
;;


(* 1. Caso: Scala valida (3, 4, 5, 6, 7) semi misti *)
assert (straight (Card(3,S), Card(4,H), Card(5,D), Card(6,C), Card(7,S)) = true);;

(* 2. Caso: Scala valida ma non in ordine di inserimento (Re, Regina, Jack, 10, 9) *)
assert (straight (Card(13,S), Card(11,H), Card(12,D), Card(9,C), Card(10,S)) = true);;

(* 3. Caso: Quasi scala (manca un numero: 1, 2, 3, 4, 6) *)
assert (straight (Card(1,S), Card(2,H), Card(3,D), Card(4,C), Card(6,S)) = false);;

(* 4. Caso: Numeri uguali (1, 2, 2, 3, 4) - NON è una scala *)
assert (straight (Card(1,S), Card(2,H), Card(2,D), Card(3,C), Card(4,S)) = false);;

(* 5. Caso: Scala minima (Asso, 2, 3, 4, 5) *)
assert (straight (Card(1,S), Card(2,H), Card(3,D), Card(4,C), Card(5,S)) = true);;