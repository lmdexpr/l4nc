let times n f =
  let helper i = if i <> n then f i; helper (i+1) in
  helper 0

module Array = struct
  include Array
  let reduce f target =
    let len = length target - 1in
    fold_right f (sub target 1 len) target.(0)
end
