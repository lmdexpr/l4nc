(*
 * Matrix is
 * [| [| a11, a12, a13 .. a1y |],
 *    [| a21,          .. a2y |],
 *    .         .
 *    .           .
 *    [| ax1,          .. axy |] |]
 *)

type 'a matrix
exception Not_square_matrix

val make : int -> int -> 'a -> 'a matrix

val get : 'a matrix -> int * int -> 'a

val width  : 'a matrix -> int
val height : 'a matrix -> int

val of_array : 'a array -> 'a matrix

(*
val hsplit : 'a matrix -> int -> 'a matrix * 'a matrix
val vsplit : 'a matrix -> int -> 'a matrix * 'a matrix
*)

val map : ('a -> 'b) -> 'a matrix -> 'b matrix
val mapij : (int*int -> 'a -> 'b) -> 'a matrix -> 'b matrix

val bin_op : ('a -> 'b -> 'c) -> 'a matrix -> 'b matrix -> 'c matrix

val add : float matrix -> float matrix -> float matrix
val sub : float matrix -> float matrix -> float matrix
val mul : float matrix -> float matrix -> float matrix
val div : float matrix -> float matrix -> float matrix

val transpose : 'a matrix -> 'a matrix

val dot : float matrix -> float matrix -> float array

(* todo: implement *)
val determinant : 'a matrix -> unit
val det : 'a matrix -> unit

val vertical_compose : 'a matrix -> 'a matrix -> 'a matrix
val horizontal_compose : 'a matrix -> 'a matrix -> 'a matrix

val pivoting : 'a matrix -> 'a matrix

(* todo: implement *)
val lu_decomp : 'a matrix -> unit

val diagonal : float matrix -> float matrix
val diag : float matrix -> float matrix

val triu : float matrix -> float matrix
val tril : float matrix -> float matrix

(* todo: implement *)
val normalize : float matrix -> unit
