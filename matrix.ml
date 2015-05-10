open Utils

(*
 * Matrix is
 * [| [| a11, a12, a13 .. a1y |],
 *    [| a21,          .. a2y |],
 *    .         .
 *    .           .
 *    [| ax1,          .. axy |] |]
 *)

let map = Array.map << Array.map

let add lhs = Array.mapi @@ fun x -> Array.mapi << ( + ) << Array.get lhs.(x)
let (<+>) = add

let sub lhs = add lhs << map (fun e -> -e)
let (<->) = sub

let mul lhs = Array.mapi @@ fun x -> Array.mapi << ( * ) << Array.get lhs.(x)
let (<*>) = mul

let transpose m = ()

let determinant m = ()
let det = determinant

let vertical_compose = Array.mapi @@ Array.append << Array.get
let (<||>) = vertical_compose

let horizontal_compose lhs rhs = Array.append
let (<-->) = horizontal_compose

let pivoting m = ()
