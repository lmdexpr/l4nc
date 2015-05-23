open Utils

type 'a matrix = 'a array array
exception Not_square_matrix

let make = Array.make_matrix

let get a (x,y) = a.(y).(x)

let width  m = Array.length m.(0)
let height   = Array.length

let of_array a = Array.map (fun e -> [| e |]) a

(*
let hsplit = ()
let vsplit = ()
*)

let map f = Array.map (fun v -> Array.map f v)
let mapij f = Array.mapi (fun y v -> Array.mapi (fun x e -> f (x,y) e) v)

let bin_op op lhs = mapij (fun (x,y) e -> op lhs.(y).(x) e) 

let add = bin_op ( +. )
let sub = bin_op ( -. )
let mul = bin_op ( *. )
let div = bin_op ( /. )

let transpose m = mapij (fun (i,j) _ -> m.(j).(i)) m

let dot lhs rhs =
  let trhs = transpose rhs in
  let buf = mul lhs trhs   in
  Array.map (fun v -> Array.reduce (+.) v ) buf

let determinant m = ()
let det = determinant

let vertical_compose lhs rhs = Array.mapi (fun i a -> Array.append a rhs.(i)) lhs

let horizontal_compose = Array.append

let pivoting m =
  let rec max_idx c i =
    if c = 0 then i
    else max_idx (c-1) (if m.(i) > m.(c) then i else c)
  in
  let max_idx = max_idx (width m - 1) 0 in
  Array.mapi (fun i v -> Array.swap v max_idx i; v) m

let lu_decomp m = ()

let is_sqmatrix m = width m <> height m

let filter_tri op m =
  if not (is_sqmatrix m) then raise Not_square_matrix
  else mapij (fun (i,j) e -> if op i j then e else 0.) m

let diagonal = filter_tri (=)
let diag = diagonal

let triu = filter_tri (<)
let tril = filter_tri (>)

let normalize m = ()
