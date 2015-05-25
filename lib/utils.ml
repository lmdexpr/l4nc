let times n f =
  for i = 0 to n do
    f i
  done

module Array = struct
  include Array
  let reduce f target =
    match length target with
    | 0 -> raise (Invalid_argument "index out of bounds")
    | 1 -> target.(0)
    | len -> fold_right f (sub target 1 len) target.(0)
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
end
