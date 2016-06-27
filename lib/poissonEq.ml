type boundary = 
  | Upper of (float -> float) * (float -> float)
  | Lower of (float -> float) * (float -> float)

let unwrap = function
  | Upper a -> a
  | Lower a -> a

let apply b = snd (unwrap b)

let solver step ?(startx=0.) ?(starty=0.) boundaries = 
  let endx = 0 in endx
