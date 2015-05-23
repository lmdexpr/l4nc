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
    let new_x = b -$ au *.$ old_x in
    let last  = Array.length new_x in
    times last (fun () -> new_x.(i) <- new_x.(i) - Matrix.of_array al.(i) *.$ new_x.(i))
    |> fun () -> new_x
  in

  let rec run n old_x = 
    let new_x  = next_x old_x in
    let new_x  = old_x -$ Array.map (fun e -> w *. e) (new_x -$ old_x) in
    let is_end =
      Array.map abs_float @@ (new_x -$ old_x) /$ new_x
      |> Array.map (fun a -> a < e)
      |> Array.reduce (&&)
    in

    if is_end then n, new_x else run (n+1) new_x
  in 

  let run = run 0 in

  if 0. < w && w < 2. then Some (run x0) else None

let gauss_seidel = sor_method 1.0

let gauss_jordan a b =
  let m = a |$ b in
  let n = Matrix.width m in
  Array.mapi (fun i v ->
    ()
  ) m

let gauss_elim = ()
