type func =
  | X
  | FVal of float
  | Add  of func * func
  | Sub  of func * func
  | Mul  of func * func
  | Div  of func * func
  | Pow  of func * func
  | Exp  of func
  | Ln   of func

exception Not_implemented

let rec diff = function
  | Add (x1, x2) -> Add (diff x1, diff x2)
  | Sub (x1, x2) -> Sub (diff x1, diff x2)
  | Mul (x1, x2) -> Add (Mul (diff x1, x2), Mul (x1, diff x2))
  | Div (x1, x2) -> Div (Sub (Mul (diff x1, x2), Mul (x1, diff x2)), (Pow (x2, FVal (2.))))
  | Pow (x,  FVal c) -> Mul (diff x, Mul (FVal c, Pow (x, FVal (c -. 1.))))
  | Exp x  -> Mul (diff x, Exp x)
  | Ln  x  -> Div (diff x, x)
  | FVal x -> FVal 0.
  | X      -> FVal 1.
  | _      -> raise Not_implemented

let rec to_val f x = match f with
  | FVal c -> c 
  | Add (f, g) -> to_val f x +. to_val g x
  | Sub (f, g) -> to_val f x -. to_val g x
  | Mul (f, g) -> to_val f x *. to_val g x
  | Div (f, g) -> to_val f x /. to_val g x
  | Pow (f, FVal c) -> to_val f x ** c
  | Exp f -> exp (to_val f x)
  | Ln  f -> log (to_val f x)
  | X -> x
  | _ -> raise Not_implemented
