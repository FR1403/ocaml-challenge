(*Se genera 0 è testa, se genera 1 è croce*)

let hot () : string =
  match Random.int(2) with 
  | 0 -> "Testa"
  | 1 -> "Croce"
  | _ -> failwith "ciao"
;;
