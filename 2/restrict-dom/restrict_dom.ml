<<<<<<< HEAD
let restrict (f: 'a -> 'b)(p: 'a -> bool) : ('a -> 'b option) =
  fun x -> if p x then Some (f x)
  else None
;;

let f1 = restrict succ (fun x -> x>0);;
assert (f1 1 = Some 2);;
assert (f1 0 = None);;

let f2 = restrict (fun (x,y) -> x - y) (fun (x,y) -> x-y>=0);;
assert(f2 (5,2) = Some 3);;
=======
let restrict (f: 'a -> 'b) (p: 'a -> bool) : ('a -> 'b option) =
  fun x -> 
    if p x = true then Some (f x) 
    else None
;;

let f1 = restrict succ (fun x -> x>0) ;;
assert (f1 1 = Some 2);;
assert (f1 0 = None);;

let f2 = restrict (fun (x,y) -> x - y) (fun (x,y) -> x-y>=0);;
assert(f2 (5,2) = Some 3);;
>>>>>>> 0c6e9e1 (altri es livello 2)
assert(f2 (5,6) = None);;