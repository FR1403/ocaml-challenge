type card = Joker | Val of int ;;

(*Se nè il giocatore nè il mazziere hanno il Joker 
vince chi ha la carta più alta *)
let win (gamer: card) (dealer: card) : bool =
  if (( gamer != Joker && dealer != Joker ) && ((gamer < Val 1 || gamer >  Val 10) || (dealer < Val 1 || dealer > Val 10))) then 
    failwith "I valori delle carte non sono validi"
  else (
    if (gamer = Joker || dealer = Joker) then 
      match (gamer, dealer) with
      | _ , Joker-> false
      | Joker, _ -> true
      | _ -> false 
    else 
      match (gamer, dealer) with
      | Val x , Val y -> if (max x y) = x then true
      else false)
  ;;


let max (a: int) (b: int) : int =
  if a >= b then a 
  else b
;;

(* Test e assert per la funzione win *)
let () =
  (* Valori numerici: il giocatore vince se la sua carta è più alta *)
  assert (win (Val 5) (Val 3) = true);
  assert (win (Val 2) (Val 4) = false);

  (* Joker: se il giocatore ha il Joker e il mazziere no, il giocatore vince *)
  assert (win Joker (Val 1) = true);
  (* se il mazziere ha il Joker e il giocatore no, il giocatore perde *)
  assert (win (Val 1) Joker = false);
  (* se entrambi hanno il Joker, vince il mazziere => il giocatore non vince *)
  assert (win Joker Joker = false);

  print_endline "✓ highest-card-wins: tutti i test sono passati!";;