type winner = Player | Computer | Tie ;;

(*hp -> mano del giocatore in range 0-5
  gp -> ipotesi del giocatore in range 0-10
  hc -> mano del computer 
  gc -> ipotesi del computer*)
let win (hp, gp: int * int) : (int * int) * winner =
  let hc = Random.int 5+1 in
  let gc = Random.int 10+1 in
  match hp, gp, hc, gc with
  | hp, gp, hc, gc when gp = (hp+hc) && gc <> (hp+hc) -> ((hc, gc) , Player)
  | hp, gp, hc, gc when gp <> (hp+hc) && gc = (hp+hc) -> ((hc, gc), Computer)
  | hp, gp, hc, gc -> ((hc, gc), Tie)
;;

(* win(2,5);;
- : (int * int) * winner = ((0, 1), Tie)

win(2,5);;
- : (int * int) * winner = ((4, 6), Computer)

win(2,5);;
- : (int * int) * winner = ((3, 4), Player) *)