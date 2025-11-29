type season = Spring | Summer | Autumn | Winter

let squirrel_play (t: int) (s: season) : bool =
  if (t >= 0 && t <= 35) then
  (match s with
  | Spring | Autumn | Winter -> if (t >= 15 && t <= 30) then true else false
  | Summer -> if (t >= 15 && t <= 35) then true else false)
  else 
    failwith("The entered value is invalid")
;;


assert(squirrel_play 18 Winter = true);;
assert(squirrel_play 32 Spring = false);;
assert(squirrel_play 32 Summer = true);;