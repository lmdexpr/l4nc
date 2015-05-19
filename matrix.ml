open Utils

module Vector = Array

(*
 * Matrix is
 * [| [| a11, a12, a13 .. a1y |],
 *    [| a21,          .. a2y |],
 *    .         .
 *    .           .
 *    [| ax1,          .. axy |] |]
 *)

let make = Vector.make_matrix

let map = Vector.map << Vector.map

let add lhs = Vector.mapi @@ fun x -> Vector.mapi << ( + ) << Vector.get lhs.(x)
let ( +/ ) = add

let sub lhs = add lhs << map (fun e -> -e)
let ( -/ ) = sub

let mul lhs = Vector.mapi @@ fun x -> Vector.mapi << ( * ) << Vector.get lhs.(x)
let ( */ ) = mul

let transpose m = ()

let determinant m = ()
let det = determinant

let vertical_compose = Vector.mapi @@ Vector.append << Vector.get
let (<||>) = vertical_compose

let horizontal_compose lhs rhs = Vector.append
let (<-->) = horizontal_compose

let pivoting m = ()

let lu_decomp = ()
