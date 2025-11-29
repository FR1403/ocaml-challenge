let is_even (x: int) : bool = 
  match x with
  | _ when (x mod 2 = 0) -> true
  | _ -> false
;;

let win (a: int) (b: int) : int =
  match (a+b) with
  (*numero scorretto entrambi*)
  | _  when ((a < 1 || a > 5) && (b < 1 || b > 5)) -> 0
  (*se un giocatore ha scelto un numero corretto e laltro no il primo vince*)
  | _  when ((a < 1 || a > 5) && (b >= 1 || b <= 5))-> (-1)
  | _  when ((a >= 1 || a <= 5) && (b < 1 || b > 5)) -> 1
  (*vince A se gioca un numero corretto e la somma dei due numeri è pari*)
  | s  when (is_even s) -> 1
  (*vince B se gioca un numero corretto e la somma dei due numeri è dispari*)
  | _ -> -1
;;
  