let is_palindrome (l: 'a list) : bool =
  let rec reverse (s: 'a list) : 'a list = 
    match s with 
    | h :: [] -> [h]
    | h :: t -> (reverse t) @ [h] 
    | [] -> []
  in

  if l = (reverse l) then true 
  else false 
;;

assert(is_palindrome []);;
assert(is_palindrome ['a';'n';'n';'a']);;
assert(is_palindrome ['r';'a';'d';'a';'r']);;
assert(is_palindrome ['a';'n';'n';'e'] = false);;
assert(is_palindrome ['z';'a';'n';'n';'a'] = false);;