open Utils

(*
 * Matrix is
 * [| [| a11, a12, a13 .. a1y |],
 *    [| a21,          .. a2y |],
 *    .         .
 *    .           .
 *    [| ax1,          .. axy |] |]
 *)

let make = Array.make_matrix

let map f = Array.map (fun v -> Array.map f v)
let mapij f = Array.mapi (fun i -> Array.mapi (fun j e -> f (i,j) e))

let bin_op op lhs = mapij (fun (i,j) e -> op lhs.(i).(j) e) 

let add lhs = bin_op ( +. )
let sub lhs = bin_op ( -. )
let mul lhs = bin_op ( *. )
let div lhs = bin_op ( /. )

let transpose m = mapij (fun (i,j) _ -> m.(j).(i)) m

let dot lhs rhs = Array.map (fun v -> Array.reduce (+.) v ) @@ mul lhs (transpose rhs)

let diagonal m = Array.mapi (fun i () -> m.(i).(i)) m
let diag = diagonal

let determinant m = ()
let det = determinant

let vertical_compose lhs rhs = Array.mapi (fun i a -> Array.append a rhs.(i)) lhs

let horizontal_compose = Array.append

let pivoting m = ()

let lu_decomp m = ()

let normalize m = let d = diag m in Array.mapi (fun i e -> div e d.(i)) m
