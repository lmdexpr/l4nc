open Utils

type 'a matrix = 'a array array
exception Not_square_matrix
exception Not_matrix
exception Cannot_apply_dot_product

let is_matrix a = let len = Array.length a.(0) in Array.for_all (fun e -> Array.length e <> len) a

let make = Array.make_matrix
let init n m f = Array.init n (fun i -> Array.init m (f i))
let identity n = init n n (fun i j -> if i = j then 1. else 0.)

let get a (i,j) = a.(i).(j)
let get_vec = Array.get

let set a (i,j) e = a.(i).(j) <- e
let set_vec = Array.set

let modify m pos f   = set m pos @@ f @@ get m pos
let modify_vec m i f = set_vec m i @@ f @@ get_vec m i

let width  m = if is_matrix m then raise Not_matrix else Array.length m.(0)
let height   = Array.length

(*
let hsplit = ()
let vsplit = ()
*)

let map_vec  = Array.map
let mapi_vec = Array.mapi

let map f = map_vec (fun v -> map_vec f v)
let mapij f = mapi_vec (fun i v -> mapi_vec (fun j e -> f i j e) v)

let fold_right op m one = Array.fold_right op (map_vec (fun v -> Array.fold_right op v one) m) one
let reduce op m = Array.reduce op (map_vec (fun v -> Array.reduce op v) m)

let of_array a = map_vec (fun e -> [| e |]) a
let of_2d_array a = if is_matrix a then raise Not_matrix else a

let bin_op op lhs = mapij (fun i j -> op lhs.(i).(j)) 

let add = bin_op ( +. )
let sub = bin_op ( -. )
let mul = bin_op ( *. )
let div = bin_op ( /. )

let transpose m i = map_vec (fun v -> v.(i)) m
let transpose m = Array.init (width m) (transpose m)

let dot lhs rhs =
  let w = width  rhs in
  let h = height lhs in
  let n = width lhs in
  if n <> height rhs then raise Cannot_apply_dot_product;
  let c i j = Array.reduce (+.) @@ Array.init n (fun k -> lhs.(i).(k) *. rhs.(k).(j))in
  Array.init h (fun i -> Array.init w (fun j -> c i j))

let determinant m = ()
let det = determinant

let vertical_compose lhs rhs = mapi_vec (fun i v -> Array.append v rhs.(i)) lhs

let horizontal_compose = Array.append

let pivoting m =
  let max_idx = mapi_vec (fun i v -> i, Array.reduce max v) m in
  Array.fast_sort (fun (_,a) (_,b) -> compare a b) max_idx;
  Array.map (fun (i,_) -> m.(i)) max_idx

let lu_decomp m = ()

let is_sqmatrix m = width m = height m

let filter_tri op m =
  if not (is_sqmatrix m) then raise Not_square_matrix
  else mapij (fun i j e -> if op i j then e else 0.) m

let diagonal = filter_tri (=)
let diag = diagonal

let triu = filter_tri (<)
let tril = filter_tri (>)

(* No export function *)
let filter_tri = ()

let normalize m = ()

let print_matrix m =
  let w = width m in
  print_string "    ";
  for i = 1 to w do
    Printf.printf "%9d" i
  done;
  print_newline ();
  Array.iteri (fun i v ->
    Printf.printf "%3d:" (i+1);
    Array.iter (Printf.printf "%9F") v;
    print_newline ()
  ) m
