type func =
  | X
  | Y
  | FVal of float
  | Add  of func * func
  | Mul  of func * func
  | Div  of func * func
  | Pow  of func * func
  | Exp  of func
  | Ln   of func
  | Neg  of func
