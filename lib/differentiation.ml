open Type
exception Not_implemented

let rec diff ?(y_flag=false) = function
  | Add (x1, x2) -> Add (diff x1, diff x2)
  | Mul (x1, x2) -> Add (Mul (diff x1, x2), Mul (x1, diff x2))
  | Div (x1, x2) -> Div (Add (Mul (diff x1, x2), Neg (Mul (x1, diff x2))), (Pow (x2, FVal (2.))))
  | Pow (x,  FVal c) -> Mul (diff x, Mul (FVal c, Pow (x, FVal (c -. 1.))))
  | Exp x  -> Mul (diff x, Exp x)
  | Ln  x  -> Div (diff x, x)
  | Neg x  -> Neg (diff x)
  | FVal x -> FVal 0.
  | X      -> if y_flag then FVal 0. else FVal 1.
  | Y      -> if y_flag then FVal 1. else FVal 0.
  | _      -> raise Not_implemented
