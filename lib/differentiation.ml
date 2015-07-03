open Type
exception Not_implemented

let rec diff = function
  | Add (x1, x2) -> Add (diff x1, diff x2)
  | Mul (x1, x2) -> Add (Mul (diff x1, x2), Mul (x1, diff x2))
  | Div (x1, x2) -> Div (Add (Mul (diff x1, x2), Neg (Mul (x1, diff x2))), (Pow (x2, FVal (2.))))
  | Pow (x,  FVal c) -> Mul (diff x, Mul (FVal c, Pow (x, FVal (c -. 1.))))
  | Exp x  -> Mul (diff x, Exp x)
  | Ln  x  -> Div (diff x, x)
  | Neg x  -> Neg (diff x)
  | FVal x -> FVal 0.
  | X      -> FVal 1.
  | _      -> raise Not_implemented
