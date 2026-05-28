type 'a option = Some of 'a | None

let minmaxfun (f: int -> 'a) (a: int) (b: int) : ('a * 'a) option =
  let rec my_min (f: int -> 'a) (a: int) (b: int) : 'a =
    if a < b then 
      if min (f a) (f b) = f a then my_min f a (b-1)
      else my_min f (a+1) b
    else min (f a) (f b)
  in

  let rec my_max (f: int -> 'a) (a: int) (b: int) : 'a =
    if a < b then 
      if max (f a) (f b) = f a then my_max f a (b-1)
      else my_max f (a+1) b
    else max (f a) (f b)
  in
  if b - a >= 0 then Some (my_min f a b, my_max f a b)
  else None

;;

assert (minmaxfun (fun x -> x) (-2) 5 = Some (-2,5));;
assert (minmaxfun (fun x -> x) 5 (-2) = None);;
assert (minmaxfun (fun x -> x) 5 5 = Some (5,5));;
assert (minmaxfun (fun x -> x * x) (-2) 5 = Some (0,25));;

let curve x = x |> Float.of_int |> fun x -> x ** 3.0 -. 3.0 *. x;;
let arccos x = x |> Float.of_int |> Float.acos;;

assert (minmaxfun curve (-2) 2 = Some (-2.0,2.0));;
assert (minmaxfun arccos (-1) 1 = Some (0., Float.pi));;

