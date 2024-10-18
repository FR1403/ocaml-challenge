let nand1 (a : bool) (b : bool)
: bool = not (a && b)

let nand2 (a : bool) (b : bool) 
= if a 
then (*[a] è vero*) (if b (*[a] è vero*) then false 
else true)
else (*[b] è falso*) true

let nand3 (a : bool) (b : bool) 
: bool =
  match (a, b) with
  | (false, false) -> true
  | (false, true) -> true
  | (true, false) -> true
  | (true, true) -> false