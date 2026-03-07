let consensus3 ((f1: 'a -> 'b) , (f2: 'a -> 'b) , (f3: 'a -> 'b)) : 'a -> 'b option =
  let risultato x =
    let v1 = f1 x in
    let v2 = f2 x in
    let v3 = f3 x in
    if v1 = v2 || v1 = v3 then Some v1
    else if v2 = v3 then Some v2
    else if (v1 <> v2) && (v2 <> v3) && (v1 <> v3) then None
    else failwith "Undefined"
  in
  risultato
;; 

assert (try
  consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0 = None
  with _ -> true);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);;