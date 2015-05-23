let times n f =
  for i = 0 to n do
    f i
  done

module Array = struct
  include Array
  let reduce f target =
    let len = length target - 1in
    fold_right f (sub target 1 len) target.(0)
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
end
