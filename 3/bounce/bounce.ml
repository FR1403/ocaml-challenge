let bounce (n: int) : int -> int =
  fun x -> (x*2) mod n
;;