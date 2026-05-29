let rotate (n: int) (l: 'a list) : 'a list =
  let rec rot (n: int) (l: 'a list) (r: 'a list) : 'a list =
    if n > 0 then
      match l, r with
      | [], h1 :: t1 -> rot (n-1) [h1] t1
      | h :: t, h1 :: t1 -> rot (n-1) (l@[h1]) t1
      | _ -> failwith "Boh"
    else (r@l)
  in
  rot n [] l

;;

assert(rotate 0 [5;6;7;8] = [5;6;7;8]);;  
assert(rotate 1 [5;6;7;8] = [6;7;8;5]);;  
assert(rotate 2 [5;6;7;8] = [7;8;5;6]);;  
assert(rotate 3 [5;6;7;8] = [8;5;6;7]);;  
assert(rotate 4 [5;6;7;8] = [5;6;7;8]);;   