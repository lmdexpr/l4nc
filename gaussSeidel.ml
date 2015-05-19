open Utils

(** Linear Equation solver by Gauss Seidel method
 *
 *  m -> (A | b) : Matrix
 *  e is threshold.
 *)
let gauss_seidel m e =
  let a, b   = Matrix.hsplit (Matrix.pivoting m) (Matrix.width m -1) in
  let ad     = Matrix.diag a in
  let a      = a /$ ad in
  let au, al = Matrix.triu a, Matrix.tril a in
  let b      = b /$ ad.(i) in

  let next_x old_x =
    let new_x = b -$ au *.$ old_x in
    let last  = Array.length new_x - 1 in
    for i = 0 to last do
      new_x.(i) <- new_x.(i) - Matrix.of_array al.(i) *.$ new_x.(i)
    done;
    new_x
  in

  let rec run old_x = 
    let new_x = next_x old_x in
    let is_end =
      Array.map abs_float @@ (new_x -$ old_x) /$ new_x
        |> Array.map (fun a -> a < e)
        |> Array.reduce (&&)
    in
    if is_end then new_x else run new_x
  in

  run
;;

let sor_method = 0
;;
