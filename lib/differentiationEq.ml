open Logger
open Matrix
open Matrix_op

let euler_method2 ?(logger=disable_logger2) h x_max f =
  let rec inner x y = 
    begin
      logger x y;
      let y = y +. h *. f x y
      and x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y) else inner x y
    end
  in inner

let euler_method3 ?(logger=disable_logger3) h x_max f1 f2 =
  let rec inner x y z = 
    begin
      logger x y z;
      let y = y +. h *. f1 x y z
      and z = z +. h *. f2 x y z
      and x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y,z) else inner x y z
    end
  in inner

let runge_kutta2_2 ?(logger=disable_logger2) h x_max f =
  let rec inner x y =
    begin
      logger x y;
      let k1 = h *. f x y in
      let k2 = h *. f (x +. h /. 2.) (y +. k1 /. 2.) in
      let y = y +. k2 in
      let x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y) else inner x y
    end
  in inner

let runge_kutta2_3 ?(logger=disable_logger2) h x_max f =
  let rec inner x y =
    begin
      logger x y;
      let k1 = h *. f x y in
      let k2 = h *. f (x +. h /. 2.) (y +. k1 /. 2.) in
      let k3 = h *. f (x +. h)       (y +. 2. *. k2 -. k1) in
      let y = y +. (k1 +. 4. *. k2 +. k3) /. 6. in
      let x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y) else inner x y
    end 
  in inner

let runge_kutta2_4 ?(logger=disable_logger2) h x_max f =
  let rec inner x y =
    begin
      logger x y;
      let k1 = h *. f x y in
      let k2 = h *. f (x +. h /. 2.) (y +. k1 /. 2.) in
      let k3 = h *. f (x +. h /. 2.) (y +. k2 /. 2.) in
      let k4 = h *. f (x +. h)       (y +. k3) in
      let y = y +. (k1 +. 2. *. k2 +. 2. *. k3 +. k4) /. 6. in
      let x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y) else inner x y
    end
  in inner

let runge_kutta3_2 ?(logger=disable_logger3) h x_max f1 f2 =
  let rec inner x y z =
    begin
      logger x y z;
      let ky1 = h *. f1 x y z in
      let kz1 = h *. f2 x y z in
      let ky2 = h *. f1 (x +. h /. 2.) (y +. ky1 /. 2.) (z +. kz1 /. 2.) in
      let kz2 = h *. f2 (x +. h /. 2.) (y +. ky1 /. 2.) (z +. kz1 /. 2.) in
      let y = y +. ky2 in
      let z = z +. kz2 in
      let x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y,z) else inner x y z
    end
  in inner

let runge_kutta3_3 ?(logger=disable_logger3) h x_max f1 f2 =
  let rec inner x y z =
    begin
      logger x y z;
      let ky1 = h *. f1 x y z in
      let kz1 = h *. f2 x y z in
      let ky2 = h *. f1 (x +. h /. 2.) (y +. ky1 /. 2.) (z +. kz1 /. 2.) in
      let kz2 = h *. f2 (x +. h /. 2.) (y +. ky1 /. 2.) (z +. kz1 /. 2.) in
      let ky3 = h *. f1 (x +. h) (y +. 2. *. ky2 -. ky1) (z +. 2. *. kz2 -. kz1) in
      let kz3 = h *. f2 (x +. h) (y +. 2. *. ky2 -. ky1) (z +. 2. *. kz2 -. kz1) in
      let y = y +. (ky1 +. 4. *. ky2 +. ky3) /. 6. in
      let z = z +. (kz1 +. 4. *. kz2 +. kz3) /. 6. in
      let x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y,z) else inner x y z
    end 
  in inner

let rec runge_kutta3_4 ?(logger=disable_logger3) h x_max f1 f2 =
  let rec inner x y z =
    begin
      logger x y z;
      let ky1 = h *. f1 x y z in
      let kz1 = h *. f2 x y z in
      let ky2 = h *. f1 (x +. h /. 2.) (y +. ky1 /. 2.) (z +. kz1 /. 2.) in
      let kz2 = h *. f2 (x +. h /. 2.) (y +. ky1 /. 2.) (z +. kz1 /. 2.) in
      let ky3 = h *. f1 (x +. h /. 2.) (y +. ky2 /. 2.) (z +. kz2 /. 2.) in
      let kz3 = h *. f2 (x +. h /. 2.) (y +. ky2 /. 2.) (z +. kz2 /. 2.) in
      let ky4 = h *. f1 (x +. h)       (y +. ky3)       (z +. kz3)       in
      let kz4 = h *. f2 (x +. h)       (y +. ky3)       (z +. kz3)       in
      let y = y +. (ky1 +. 2. *. ky2 +. 2. *. ky3 +. ky4) /. 6. in
      let z = z +. (kz1 +. 2. *. kz2 +. 2. *. kz3 +. kz4) /. 6. in
      let x = x +. h in
      let e = h *. 0.1 in
      if x +. e > x_max then (x,y,z) else inner x y z
    end
  in inner

(*
 * k2 y_i+1 + k1 y_i + k0 y_i-1 = C x_i
 *)
let rec boundary_problem min max fa fb fc n =
  let h = (fst max -. fst min) /. float n in
  let n = n - 1 in

  let x i  = fst min +. h *. (float i) in

  (*
  let k2 i = 1. /. (h *. h) +. fa (x i) /. (2. *. h)
  and k1 i = fb (x i) -. 2. /. (h *. h)
  and k0 i = 1. /. (h *. h) -. fa (x i) /. (2. *. h) in
  *)
  let k2 i = 1. +. h *. fa (x i) /. 2.
  and k1 i = h  *. h *. fb (x i) -. 2.
  and k0 i = 1. -. h *. fa (x i) /. 2.
  and fc x = h *. h *. fc x in

  let difference_eq i j =
    if i = j-1 then k0 j else
    if i = j   then k1 j else
    if i = j+1 then k2 j else 0.
  and coefficient i _ =
    fc (float i) -. (
      if i = 0   then snd min *. k0 i else
      if i = n-1 then snd max *. k2 i else 0.) in

  Matrix.init n n difference_eq, Matrix.init n 1 coefficient
