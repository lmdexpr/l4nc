let (>>) f g x = g (f x)
let (<<) f g x = f (g x)

module Array = struct
  include Array
  let reduce f target =
    let len = length target - 1in
    fold_right f (sub target 1 len) target.(0)
end
