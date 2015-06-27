open Differentiation
exception Not_convergence
exception Divergence

let newton_method f f' x0 logger e e1 = 
  let next x = x -. (to_val f x /. to_val f' x) in
  let rec helper n px x =
    let nx = next x in
    if e1 > abs_float @@ to_val f' x then
      raise Divergence
    else if abs_float (nx -. x) > abs_float (x -. px) then
      raise Not_convergence
    else if e > abs_float @@ (x -. px) /. x then x
    else begin
      logger n x;
      helper (n+1) x nx
    end
  in helper 0 x0 @@ next x0

let newton_method ?(logger=(fun _ _ -> ())) ?(epsilon=0.00001) ?(epsilon1=0.00001) exp x0 =
  newton_method exp (diff exp) x0 logger epsilon epsilon1

let bisection_method ?(logger=(fun _ _ -> ())) ?(epsilon=0.00001) exp x1 h =
  let predicate x h = to_val exp x *. to_val exp (x+.h) < 0. in
  let rec helper x h = 
    let h      = h /. 2. in
    let next_x = x +. if predicate x h then 0. else h in
    if epsilon > h then next_x
    else helper next_x h
  in
  if predicate x1 h then helper x1 h
  else invalid_arg "should be f(x1)f(x1+h) < 0"
