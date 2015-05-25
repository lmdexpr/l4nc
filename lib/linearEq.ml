open Utils
open Matrix_op

(** Linear Equation solver by Gauss Seidel method
 *
 *  m -> (A | b) : Matrix
 *  e is threshold.
 *)

let sor_method w e x0 a b =
  let ad = Matrix.diag a in
  let a  = Matrix.mapij (fun (i,_) e -> e /. Matrix.get ad (i,i)) a in
  let au = Matrix.triu a in
  let al = Matrix.tril a in

  let b = Array.mapi (fun i e -> e /. Matrix.get ad (i,i)) b in
  let b = Matrix.of_array b in

  let next_x old_x =
    let new_x = b -$ (Matrix.of_array (au *.$ old_x)) in
    let last  = Matrix.height new_x in
    times last (fun i ->
      let buf = Matrix.of_array (Matrix.get_vec al i) in
      let buf = buf *.$ new_x in
      Matrix.modify new_x (0,i) (fun lhs -> lhs -. buf.(0))
      );
    new_x
  in

  let rec run n old_x = 
    let new_x  = next_x old_x in
    let new_x  = old_x -$ Matrix.map (fun e -> w *. e) (new_x -$ old_x) in
    let is_end =
      Matrix.map abs_float @@ (new_x -$ old_x) /$ new_x
      |> Matrix.map (fun a -> a < e)
      |> Matrix.reduce (&&)
    in

    if is_end then n, new_x else run (n+1) new_x
  in 

  let run = run 0 in

  if 0. < w && w < 2. then Some (run (Matrix.of_array x0)) else None

let gauss_seidel = sor_method 1.0

let gauss_jordan step m =
  let last = Matrix.width m - 2 in
  times last (step last m);
  Matrix.mapi_vec (fun i v -> [| v.(last) /. v.(i) |]) m

let step n m k =
  times n (fun i ->
    let c = Matrix.get m (k,i) /. Matrix.get m (k,k) in
    for j = k to n + 1 do
      let c' = Matrix.get m (j,k) *. c in
      Matrix.modify m (j,i) (fun lhs -> lhs -. c')
    done)

let gauss_jordan           = gauss_jordan step
and gauss_jordan_with_norm = gauss_jordan (fun n m k ->
  for i = 0 to n + 1 do
    Matrix.modify m (k,i) (fun lhs -> lhs /. Matrix.get m (k, k))
  done;
  step n m k)

(* unexport variables *)
let step = ()

let gauss_elim = ()
