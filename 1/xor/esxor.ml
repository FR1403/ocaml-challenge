(*
b1     | b2   
true true false
true false true
false true true
false false false
*)
let xor1 : bool -> bool -> bool = 
    fun b1 b2 -> 
        not b1 && b2 || b1 && not b2 


let xor2 : bool -> bool -> bool = 
    fun b1 b2->
        if b1 
            then 
                if b2 then false else true (*= not b2*)
            else
                if b2 then true else false (*=b2*)

let xor3 : bool -> bool -> bool =
    fun b1 b2->
        match(b1, b2) with 
            | true, true -> false
            | false, true -> true
            | true, false -> true
            | false, false -> false