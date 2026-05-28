let has_one (n: int) : bool =
  let is_one (l: int) : bool =
    if l = 1 then true 
    else false 
  in 
  let rec check (x: int) : bool =
    if x = 0 then false 
    else if x < 10 && is_one x then true
    else if is_one (x mod 10) then true
    else if check (x/10) then true
    else false  
  in
  if n >= 0 then check n
  else failwith "Numero minore di zero"

;;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;