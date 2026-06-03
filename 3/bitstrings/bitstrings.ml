type bitstring = E | Z of bitstring | U of bitstring ;;

(*
  - E -> empty string (stringa vuota)
  - Z s -> string made by a 0 followed by s (01001001)
  - U s -> string made by a 1 followed by s
*)

let b_string = Z(U(Z(U(U(E)))));;
let b_string2 = Z(U(Z(Z(U(Z(E))))));;

let bit = "01001";;

let rec string_of_bitstring (b: bitstring) : string =
  match b with
  | Z s -> "0" ^ string_of_bitstring s
  | U s -> "1" ^ string_of_bitstring s
  | E -> ""
;;

let rec list_of_bitstring (b: bitstring) : int list =
  match b with
  | Z s -> [0] @ list_of_bitstring s
  | U s -> [1] @ list_of_bitstring s
  | E -> []
;;

let bitstring_of_string (s: string) : bitstring =
  let rec transform (s: string) (i: int) : bitstring =
    if i < (String.length s) then
      match s.[i] with
      | '0' -> Z((transform s (i+1)))
      | '1' -> U((transform s (i+1)))
      | _ -> E
    else E
  in 
  transform s 0

;;

let len (b: bitstring) : int =
  let rec count (b: string) : int =
    String.length b
  in
  count (string_of_bitstring b)
;;

let is_zero (n: int) : bool =
  n = 0
;;

let countZ (b: bitstring) : int =
  List.filter is_zero (list_of_bitstring b) |> List.length 
;;

let countU (b: bitstring) : int =
  List.filter (fun x -> not (is_zero x)) (list_of_bitstring b) |> List.length
;;

let concat (b1: bitstring) (b2: bitstring) : bitstring =
  let string_complete = (string_of_bitstring b1 ) ^ (string_of_bitstring b2) in
  bitstring_of_string string_complete
;;

let equals (b1: bitstring) (b2: bitstring) : bool =
  b1 = b2
;;

let tl (b: bitstring) : bitstring =
  match b with 
  | U s -> s
  | Z s -> s 
  | E -> E
;;

let rec prefix (b1: bitstring) (b2: bitstring) : bool = 
  match b1, b2 with
  | Z s1, Z s2 -> prefix s1 s2 
  | U s1, U s2 -> prefix s1 s2
  | E , _ -> true 
  | _ , _ -> false
;;

(* ZZ ZUZ *)
let substring (b1: bitstring) (b2: bitstring) : bool = 
  let rec check (b1: bitstring) (m1: bitstring) (b2: bitstring) (m2: bitstring) : bool =
    match b1 with 
    | Z s1 -> (match b2 with 
              | Z s2 -> check s1 b1 s2 b2
              | U s2 -> check m1 m1 s2 b2
              | E -> false) 
    | U s1 -> (match b2 with
              | Z s2 -> check m1 m1 s2 b2
              | U s2 -> check s1 b1 s2 b2
              | E -> false)
    | E -> true
  in

  check b1 b1 b2 b2
;;

(* ========================== *)
(*          TESTS             *)
(* ========================== *)

let () =
  (* Asserts per string_of_bitstring *)
  assert (string_of_bitstring E = "");
  assert (string_of_bitstring (Z (U (Z E))) = "010");

  (* Asserts per list_of_bitstring *)
  assert (list_of_bitstring E = []);
  assert (list_of_bitstring (Z (U (Z E))) = [0; 1; 0]);

  (* Asserts per bitstring_of_string *)
  assert (bitstring_of_string "" = E);
  assert (bitstring_of_string "010" = Z (U (Z E)));

  (* Asserts per len *)
  assert (len E = 0);
  assert (len (Z (U (Z E))) = 3);

  (* Asserts per countZ *)
  assert (countZ E = 0);
  assert (countZ (Z (U (Z E))) = 2);

  (* Asserts per countU *)
  assert (countU E = 0);
  assert (countU (Z (U (Z E))) = 1);

  (* Asserts per concat *)
  assert (concat (Z E) (U E) = Z (U E));
  assert (concat E (Z (U E)) = Z (U E));

  (* Asserts per equals *)
  assert (equals (Z (U E)) (Z (U E)) = true);
  assert (equals (Z (U E)) (U (Z E)) = false);

  (* Asserts per tl *)
  assert (tl (Z (U E)) = U E);
  assert (tl E = E);

  (* Asserts per prefix *)
  assert (prefix E (Z (U E)) = true);
  assert (prefix (Z E) (Z (U E)) = true);
  assert (prefix (U E) (Z (U E)) = false);
  assert (prefix (Z (U E)) (Z E) = false);

  (* Asserts per substring *)
  (* Questi assert verificheranno la correttezza della tua funzione substring *)
  assert (substring E (Z (U (Z E))) = true);
  assert (substring (U (Z E)) (Z (U (Z E))) = true);
  assert (substring (Z (Z E)) (Z (U (Z E))) = false);
  assert (substring (Z (U (Z E))) (U (Z E)) = false);
  
  print_endline "Tutti i test sono passati con successo!"
;;