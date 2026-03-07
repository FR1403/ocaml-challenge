(*La funzione dice lancia un dado truccato in cui la probabilità che esca 
6 è il parametro della funzione*)
let dice (prob: int) : int =
  let rand = (Random.int 100) in
  if rand < prob then 6 
  else Random.int 5+1
;;
