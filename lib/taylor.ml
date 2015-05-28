open Differentiation
open Utils

let rec fact acc n =
  if n = 0 then acc
  else fact (acc *. float n) (n-1)

let fact = fact 1.

let approximate x0 f logger x n = 
  let rec approximate acc df k = 
    if k > n then acc
    else begin
      let acc = acc +. (to_val df x0 /. fact k) *. ((x -. x0) ** float k) in
      logger acc;
      approximate acc (diff f) (k+1)
    end
  in
  approximate 0. f 0

(* avoid value restriction *)
let mac_expa f logger = approximate 0. f logger
