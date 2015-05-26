type func =
  | X
  | FVal of float
  | FunOrVal of func
  | Add  of func * func
  | Mul  of func * func
  | Div  of func * func
  | Pow  of func * func
  | Exp  of func
  | Ln   of func

exception Not_implemented

let is_val = function
  | FVal _ -> true
  | _      -> false
 
let rec diff = function
  | Add (x1, x2) -> Add (diff x1, diff x2)
  | Mul (x1, x2) -> Add (Mul (diff x1, x2), Mul (x1, diff x2))
  | Pow (X,  FVal c) -> Mul (FVal c, Pow (X, FVal (c -. 1.)))
  | Exp x  -> Mul ((if is_val x then x else diff x), Exp x)
  | Ln  x  -> Div ((if is_val x then x else diff x), x)
  | FVal x -> FVal 0.
  | X      -> FVal 1.
  | _      -> raise Not_implemented

let rec to_val = function
  | FVal x -> x
  | Add (x1, x2) -> to_val x1 +. to_val x2
  | Mul (x1, x2) -> to_val x1 *. to_val x2
  | Div (x1, x2) -> to_val x1 /. to_val x2
  | Exp x -> exp (to_val x)
  | Ln  x -> log (to_val x)
  | _ -> raise Not_implemented

let to_val x = function
  | X -> x
  | x -> to_val x
