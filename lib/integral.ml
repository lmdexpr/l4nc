open Type
open Utils
exception Not_implemented

let x a h i = float i *. h +. float a
let y f     = Eval.to_val f

let rectangles n f a b =
  let h = float (b - a) /. float n in
  let y = y f <~ x a h in 
  h *. Array.reduce (+.) (Array.init n y)

let trapezoidal n f a b =
  let h = float (b - a) /. float n in
  let y = y f <~ x a h in 
  let h = h /. 2. in
  let iter i = y i +. y (i+1) in
  h *. Array.reduce (+.) (Array.init n iter)

let simpson n f a b =
  let h = float (b - a) /. float n in
  let y = y f <~ x a h in 
  let h = h /. 3. in
  let iter i = y i +. (4. *. y (i+1)) +. y (i+2) in
  let iter = iter <~ ( * ) 2 in
  if n mod 2 <> 0 then invalid_arg "simpson's n should be even number."
  else h *. Array.reduce (+.) (Array.init (n / 2) iter)
