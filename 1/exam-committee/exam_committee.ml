type vote = StrongReject | WeakReject | WeakAccept | StrongAccept

let decide_exam (v1: vote) (v2: vote) (v3: vote): bool =
     match (v1, v2, v3) with
     | _, _, StrongReject -> false
     | StrongReject, _, _ -> false
     | _, StrongReject, _ -> false 
     | WeakReject, WeakReject, _ -> false
     | WeakReject, _, WeakReject -> false
     | _, WeakReject, WeakReject -> false
     | _, _, _ -> true
;;