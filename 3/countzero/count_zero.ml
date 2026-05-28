let rec countzero (f: int -> int) (a: int) (b: int) : int =
  let rec acc (f: int -> int) (a: int) (b: int) (n: int) : int = 
    if a <= b then 
      if f a = 0 then acc f (a+1) b n+1
      else acc f (a+1) b n 
    else 0
  in
  acc f a b 0
  
;;

assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);; 