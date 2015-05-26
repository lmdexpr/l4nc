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
    let last  = width x in
    times last (fun i ->
      let buf = of_array @@ get_vec al i in
      let buf = buf *.$ transpose x in
      modify x (i,0) (fun lhs -> (lhs -. get buf (0,0)) /. ad i);
      );
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

  let run = run 0 in

  if 0. < w && w < 2. then Some (run (of_array x0)) else None

let gauss_seidel = sor_method 1.0

let gauss_jordan step m =
  let last = width m - 2 in
  times last (step last m);
  mapi_vec (fun i v -> [| v.(last) /. v.(i) |]) m

let step n m k =
  times n (fun i ->
    let c = get m (i,k) /. get m (k,k) in
    for j = k to n + 1 do
      let c' = get m (k,j) *. c in
      modify m (i,j) (fun lhs -> lhs -. c')
    done)

  let gauss_jordan_with_norm = gauss_jordan (fun n m k ->
  for i = 0 to n + 1 do
    modify m (i,k) (fun lhs -> lhs /. get m (k, k))
  done;
  step n m k)
let gauss_jordan           = gauss_jordan step

(* No export function *)
let step = ()

let gauss_elim = ()
