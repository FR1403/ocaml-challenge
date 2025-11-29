let minmax3 (a: 'a) (b: 'a) (c: 'a) : 'a * 'a =
  let ma = max(max a b) c in
  let mi = min(min a b) c in
  mi, ma
;;


let max (a: 'a) (b: 'a) : 'a =
  if a >= b then a 
  else b
;;

let min (a: 'a) (b: 'a) : 'a =
  if a <= b then a 
  else b
;;