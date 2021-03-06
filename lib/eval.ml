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
  | Y      -> Y

let eval f =
  let g = ref @@ eval f in
  let f = ref f in
  while !f <> !g do
    f := !g;
    g := eval !g
  done;
  !g

let rec to_val f ?(y=0.) x = match f with
  | FVal c -> c 
  | Add (f, g) -> to_val f x ~y +. to_val g x ~y
  | Mul (f, g) -> to_val f x ~y *. to_val g x ~y
  | Div (f, g) -> to_val f x ~y /. to_val g x ~y
  | Pow (f, FVal c) -> to_val f x ~y ** c
  | Exp f -> exp (to_val f x ~y)
  | Ln  f -> log (to_val f x ~y)
  | Neg f -> -1. *. to_val f x ~y
  | X -> x
  | Y -> y
  | _ -> raise Not_implemented
