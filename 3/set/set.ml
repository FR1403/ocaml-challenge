let mem (e: 'a) (l: 'a list) : bool =
  let rec check (e: 'a) (l: 'a list) : bool =
    match l with
    | h :: t -> if h = e then true else check e t
    | [] -> false
  in
  check e l
;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;

let rec subseteq (xl: 'a list) (yl: 'a list) : bool =
  match xl with 
  | h1 :: t1 -> if mem h1 yl then subseteq t1 yl else false
  | [] -> true
;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;

let seteq (l1: 'list) (l2: 'a list) : bool =
  if List.length l1 = List.length l2 then 
    let rec check (l1: 'a list) (l2: 'a list) : bool =
      match l1 with
      | h :: t -> if mem h l2 then check t l2 else false
      | [] -> true 
    in
    check l1 l2
  else false
;;

let seteq2 (l1: 'list) (l2: 'a list) : bool =
  if List.length l1 = List.length l2 then 
    subseteq l1 l2
  else false
;;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;

assert(seteq2 [1;5;3] [1;3;5]);;
assert(seteq2 [1;5;2] [1;3;5] = false);;
assert(seteq2 [[1;2]] [[2;1]] = false);;
assert(seteq2 [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;

let dup (l: 'a list) : bool =
  let rec count (e: 'a) (l: 'a list) : int = 
    match l with
    | h :: t -> if h = e then 1 + count e t else count e t 
    | [] -> 0
  in
  let rec check_set (l: 'a list) : bool = 
    match l with 
    | h :: t -> if count h l <> 1 then true else check_set t
    | [] -> false
  in

  check_set l
;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;

let mkset (l: 'a list) : 'a list =
  (* let rec count (e: 'a) (l: 'a list) : int = 
    match l with
    | h :: t -> if h = e then 1 + count e t else count e t 
    | [] -> 0
  in *)
  let rec clean (l: 'a list) (l2: 'a list) : 'a list =
    match l with
    | h :: t -> if mem h l2 then clean t l2 else clean t (l2 @ [h])
    | [] -> l2
  in
  if dup l then clean l []
  else l
;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;

let union (l1: 'a list) (l2: 'a list) : 'a list =
  let rec new_set (l1: 'a list) (l2: 'a list) (n: 'a list) : 'a list =
    match l1 with
    | h :: t -> new_set t l2 (n @ [h]) 
    | [] -> match l2 with
            | h1 :: t1 -> if mem h1 n then new_set l1 t1 n else new_set l1 t1 (n @ [h1])
            | [] -> n
  in

  new_set l1 l2 []
   
;;

let inter (l1: 'a list) (l2: 'a list) : 'a list =
  let rec new_set (l1: 'a list) (l2: 'a list) (n: 'a list) : 'a list =
    match l1 with 
    | h :: t -> if mem h l2 then new_set t l2 (n @ [h]) else new_set t l2 n
    | [] -> match l2 with
            | h :: t -> if (mem h l1 && not(mem h n)) then new_set l1 t (n @ [h]) else new_set l1 t n
            | [] -> n
  in
  
  new_set l1 l2 []
;;

let diff (l1: 'a list) (l2: 'a list) : 'a list =
  let rec new_set (l1: 'a list) (l2: 'a list) (n: 'a list) : 'a list =
    match l1 with
    | h :: t -> if not(mem h l2) then new_set t l2 (n @ [h]) else new_set t l2 n
    | [] -> n
  in
  new_set l1 l2 []
;;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;

assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;

assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;

let dsum (xl: 'a list) (yl: 'a list) : (int * 'a) list =
  let rec new_set (l1: 'a list) (l2: 'a list) (n: (int * 'a) list) : (int * 'a) list =
    match l1 with 
    | h :: t -> new_set t l2 (n @ [(0, h)])
    | [] -> (match l2 with 
            | h :: t -> new_set l1 t (n @ [(1, h)]) 
            | [] -> n)
  in
  new_set xl yl []
;;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;

(* let powset (l: 'a list) : 'a list list =
  let rec pow (n: int) (exp: int) : int =
    if exp < 1 then 1
    else n * pow n (exp-1)
  in
  let rec new_set (l: 'a list) (n: 'a list list) (aux: 'a list) : 'a list list =
    if ((List.length n)-1) < pow 2 ((List.length l)-1) then
      (match l with
      | h :: t -> new_set t (n @ [[h]]) (aux @ [h])
      | [] -> new_set aux n [])
    else n
  in
  [] :: new_set l [] []
;; *)


let rec powset (l: 'a list) : 'a list list =
  match l with
  | [] -> [[]]
  | h :: t -> 
      let ps_t = powset t in
      ps_t @ List.map (fun subset -> h :: subset) ps_t
;;

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;