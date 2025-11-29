let mux1 (s0: bool) (a: bool) (b: bool) : bool =
  (a && s0) || (b && not s0)
;;

let mux2 (s0: bool) (a: bool) (b: bool) : bool =
  if (s0) then a
  else b
;;

let mux3 (s0: bool) (a: bool) (b: bool) : bool=
  match (s0, a, b) with
  | true, _, _ -> a
  | false, _, _ -> b
;;

let mux4 (s0: bool) (s1: bool) (a0: bool) (a1: bool) (a2: bool) (a3: bool): bool =
  match (s0, s1) with 
  | false, false -> a0
  | false, true -> a1
  | true, false -> a2 
  | true, true -> a3
;;

let mux4_2 (s0: bool) (s1: bool) (a0: bool) (a1: bool) (a2: bool) (a3: bool): bool =
  match s0 with
  | false -> mux3 (not s1) a0 a1 
  | true -> mux3 (not s1) a2 a3
;;
  

assert(mux4_2 false false false true false true = false);;
assert(mux4_2 false true false true false true = true);;
assert(mux4_2 true false false true false true = false);;
assert(mux4_2 true true false true false true = true);;
  