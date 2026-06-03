
(**)
let f0 (s: string) : bool =
  let len = String.length s in
  (* Controlla se tutti i caratteri della stringa appartengono all'alfabeto {0, 1} *)
  let rec all_binary i =
    if i >= len then true
    else
      let c = s.[i] in
      if c = '0' || c = '1' then all_binary (i + 1)
      else false
  in
  (* La stringa deve contenere solo '0' e '1', e se non è vuota deve terminare con '1' (non con '0') *)
  all_binary 0 && (len = 0 || s.[len - 1] = '1')
;;

