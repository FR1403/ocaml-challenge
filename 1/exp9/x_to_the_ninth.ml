let square (x: int) : int = x*x;;

let exp9 (x: int) : int =
  square (square (square x)) * x ;;