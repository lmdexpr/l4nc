open Type
exception Not_implemented

let rec eval = function
  | Add (FVal a, FVal b) -> FVal (a +. b)
  | Mul (FVal a, FVal b) -> FVal (a *. b)
  | Div (FVal a, FVal b) -> FVal (a /. b)
  | Add (FVal 0., x) -> eval x
  | Add (x, FVal 0.) -> eval x
  | Mul (FVal 0., x) -> FVal 0.
  | Mul (x, FVal 0.) -> FVal 0.
  | Mul (FVal 1., x) -> eval x
  | Mul (x, FVal 1.) -> eval x
  | Div (x, FVal 1.) -> eval x
  | Pow (x, FVal 0.) -> FVal 1.
  | Pow (x, FVal 1.) -> eval x
  | Add (x1, x2) -> Add (eval x1, eval x2)
  | Mul (x1, x2) -> Mul (eval x1, eval x2)
  | Div (x1, x2) -> Div (eval x1, eval x2)
  | Pow (x1, x2) -> Pow (eval x1, eval x2)
  | Exp x  -> Exp (eval x)
  | Ln  x  -> Ln  (eval x)
  | Neg x  -> Neg (eval x)
  | FVal x -> FVal x
  | X      -> X

let eval f =
  let g = ref @@ eval f in
  let f = ref f in
  while !f <> !g do
    f := !g;
    g := eval !g
  done;
  !g

let rec to_val f x = match f with
  | FVal c -> c 
  | Add (f, g) -> to_val f x +. to_val g x
  | Mul (f, g) -> to_val f x *. to_val g x
  | Div (f, g) -> to_val f x /. to_val g x
  | Pow (f, FVal c) -> to_val f x ** c
  | Exp f -> exp (to_val f x)
  | Ln  f -> log (to_val f x)
  | Neg f -> -1. *. to_val f x
  | X -> x
  | _ -> raise Not_implemented
