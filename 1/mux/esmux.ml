let mux2: bool -> bool -> bool -> bool =
  fun s0 a b ->
    if s0 
      then 
        a
      else
        b

let mux4 : bool -> bool -> bool -> bool -> bool ->
bool -> bool =
  fun s0 s1 a b c d ->
    mux2 s1 
    (mux2 s0
      d
      c)
    (mux2 s0
      b 
      a)
  ;;


assert (mux4 true true true true true false = 
false) ;;
assert (mux4 true false true false true false = 
false) ;;
assert (mux4 false true true false true true = 
false) ;;
assert (mux4 false false false true true true = 
false) ;;