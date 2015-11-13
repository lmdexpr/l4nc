open Type
open Utils
exception Not_implemented

let x a h i = float i *. h +. float a
let y f a h = Eval.to_val f <~ x a h
let h n a b = float (b - a) /. float n

let rec sum_iter acc n f =
  if n < 0 then acc
  else sum_iter (acc +. f n) (n-1) f

let sum_iter = sum_iter 0.

let rectangles n f a b =
  let h = h n a b in
  h *. sum_iter (n-1) (y f a h)

let trapezoidal n f a b =
  let h = h n a b in
  let y = y f a h in
  h /. 2. *. sum_iter (n-1) (fun i -> y i +. y (i+1))

let simpson n f a b =
  let h = h n a b in
  let y = y f a h in
  let iter i = y i +. (4. *. y (i+1)) +. y (i+2) in
  if n mod 2 <> 0 then invalid_arg "simpson's n should be even number."
  else h /. 3. *. sum_iter (n / 2 - 1) (iter <~ ( * ) 2)

let x = ()
let y = ()
let h = ()
let sum_iter = ()
