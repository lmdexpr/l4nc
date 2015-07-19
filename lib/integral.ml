open Type
open Utils
exception Not_implemented

let x a h i = float i *. h +. float a
let y f a h = Eval.to_val f <~ x a h
let h n a b = float (b - a) /. float n

let sum = Array.reduce (+.)

let rectangles n f a b =
  let h = h n a b in
  h *. sum (Array.init n @@ y f a h)

let trapezoidal n f a b =
  let h = h n a b /. 2. in
  let y = y f a h in
  let iter i = y i +. y (i+1) in
  h *. sum (Array.init n iter)

let simpson n f a b =
  let h = h n a b /. 3. in
  let y = y f a h in
  let iter i = y i +. (4. *. y (i+1)) +. y (i+2) in
  let iter   = iter <~ ( * ) 2 in
  if n mod 2 <> 0 then invalid_arg "simpson's n should be even number."
  else h *. sum (Array.init (n / 2) iter)

let x = ()
let y = ()
let h = ()
let sum = ()
