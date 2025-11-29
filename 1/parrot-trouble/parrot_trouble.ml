let parrot_trouble (b: bool) (t: int) : bool option =
  if( t >= 0 && t <= 23) then
    if (t < 7 || t > 20) then 
      Some b 
    else
      Some false
  else None
;;