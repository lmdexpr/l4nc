let times n f =
  for i = 0 to n do
    f i
  done

module Array = struct
  include Array
  let reduce f target =
    match length target with
    | 0 -> raise (Invalid_argument "cannot apply reduce")
    | 1 -> target.(0)
    | len -> fold_left f target.(0) (sub target 1 (len - 1))
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  let for_all p target = reduce (&&) (map p target)
end
