open Utils
open Matrix
open Matrix_op

(** Linear Equation solver by Gauss Seidel method
 *
 *  m -> (A | b) : Matrix
 *  e is threshold.
 *)

let sor_method w e x0 a b =
  let ad i = get (diag a) (i,i) in

  let au = triu a in
  let al = tril a in

  let next_x x =
    let x     = b -$ au *.$ x in
    let last  = height x - 1 in
    times last (fun i -> modify x (i,0) (fun lhs -> (lhs -. get (al *.$ x) (i,0)) /. ad i));
    x
  in

  let rec run n old_x = 
    let new_x  = next_x old_x in
    let new_x  = old_x +$ map (fun e -> w *. e) (new_x -$ old_x) in
    let is_end =
      map abs_float @@ (new_x -$ old_x) /$ new_x
      |> map (fun a -> a < e)
      |> reduce (&&)
    in

    if is_end then n, new_x else run (n+1) new_x
  in 

  let run = run 1 in

  if not (0. < w && w < 2.) then invalid_arg "weight is out of correct range"
  else run (of_array x0)

let gauss_seidel = sor_method 1.0

let gauss_elim n step post_proc m =
  let last = width m - 1 - n in
  times last (step last n m);
  post_proc m

let step last n m k =
  times last (fun i ->
    if i = k then ()
    else begin
      let c = get m (i,k) /. get m (k,k) in
      for j = k to last + n do
        let f lhs = lhs -. get m (k,j) *. c in
        modify m (i,j) f
      done
    end)

let step_norm last n m k =
  let c = get m (k, k) in
  modify_vec m k @@ Array.map (fun lhs -> lhs /. c);
  step last n m k

let gauss_jordan = gauss_elim 1

let gauss_jordan = gauss_jordan step (mapi_vec (fun i v -> [| v.(Array.length v - 1) /. v.(i) |]))

and gauss_jordan_with_norm = gauss_jordan step_norm (map_vec (fun v -> [| v.(Array.length v - 1) |])) 

let gauss_elim m b =
  let h = height m in
  let m = m |$ identity h in
  let post_p v =
    let len = Array.length v / 2 in
    Array.init len @@ fun i -> v.(len+i)
  in
  let post_p   = map_vec post_p in
  let inv_a    = gauss_elim h step_norm post_p m in
  inv_a *.$ b

  (* No export function *)
let step = ()
let step_norm = ()
