open Differentiation
open Eval
open Logger
exception Not_convergence
exception Divergence

let newton_method f f' x0 logger e e1 = 
  let next x = x -. (to_val f x /. to_val f' x) in
  let rec helper n px x =
    let nx = next x in begin
      logger n x;
      if e1 > abs_float (to_val f' nx) then
        raise Divergence
      else if abs_float (nx -. x) > abs_float (x -. px) then
        raise Not_convergence
      else if e > abs_float @@ (x -. px) /. x then
        x
      else
        helper (n+1) x nx
    end
  in helper 1 x0 (next x0)

let newton_method ?(logger=disable_logger2) ?(epsilon=0.00001) ?(epsilon1=0.0000001) exp x0 =
  newton_method exp (diff exp) x0 logger epsilon epsilon1

let bisection_method ?(logger=disable_logger2) ?(epsilon=0.00001) exp x1 h =
  let predicate x h = to_val exp x *. to_val exp (x+.h) < 0. in
  let next x h = x +. if predicate x h then 0. else h in
  let rec helper n x h = 
    let h = h /. 2.  in
    let x = next x h in
    begin
      logger n x;
      if epsilon > h then x else helper (n+1) x h
    end
  in
  if predicate x1 h then
    helper 1 x1 h
  else
    invalid_arg "should be f(x1)f(x1+h) < 0"
