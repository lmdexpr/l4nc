open Utils

type 'a matrix = 'a array array
exception Not_square_matrix

let make = Array.make_matrix

let get a (x,y) = a.(y).(x)
let get_vec = Array.get

let set a (x,y) e = a.(y).(x) <- e
let modify a pos f = set a pos (f (get a pos))

let width  m = Array.length m.(0)
let height   = Array.length

(*
let hsplit = ()
let vsplit = ()
*)

let map_vec  = Array.map
let mapi_vec = Array.mapi

let map f = map_vec (fun v -> map_vec f v)
let mapij f = mapi_vec (fun y v -> mapi_vec (fun x e -> f (x,y) e) v)

let fold_right op m one = Array.fold_right op (map_vec (fun v -> Array.fold_right op v one) m) one
let reduce op m = Array.reduce op (map_vec (fun v -> Array.reduce op v) m)

let of_array a = map_vec (fun e -> [| e |]) a

let bin_op op lhs = mapij (fun (x,y) e -> op lhs.(y).(x) e) 

let add = bin_op ( +. )
let sub = bin_op ( -. )
let mul = bin_op ( *. )
let div = bin_op ( /. )

let transpose m i = map_vec (fun v -> v.(i)) m
let transpose m = Array.init (width m) (transpose m)

let dot lhs rhs =
  let trhs = transpose rhs in
  let buf = mul lhs trhs   in
  map_vec (fun v -> Array.reduce (+.) v ) buf

let determinant m = ()
let det = determinant

let vertical_compose lhs rhs = mapi_vec (fun i v -> Array.append v rhs.(i)) lhs

let horizontal_compose = Array.append

let pivoting m =
  let rec max_idx c i =
    if c = 0 then i
    else max_idx (c-1) (if m.(i) > m.(c) then i else c)
  in
  let max_idx = max_idx (width m - 1) 0 in
  mapi_vec (fun i v -> Array.swap v max_idx i; v) m

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

let print_matrix m =
  let w = width m in
  print_string "   ";
  for i = 1 to w do
    Printf.printf "%8d" i
  done;
  Array.iteri (fun i v ->
    Printf.printf "%3d" (i+1);
    Array.iter (Printf.printf "%8F") v;
    print_newline ()
  ) m
