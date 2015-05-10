(*
 * Linear Equation solver by Gauss Seidel method
 *
 * m  -> (A | b)
 *)
let gauss_seidel m x0 =
    let w  = Matrix.width  m in
    let h  = Matrix.height m in

    let xs = ref @@ x0 <||> Matrix.null (w - 1) h in

    let a, b   = Matrix.hsplit (Matrix.pivoting m) (w-1) in
    let au, al = Matrix.lu_decomp a in

    let gauss_seidel k = b <-> (au <*> !xs.(k)) <-> (al <*> !xs.(k+1)) in
