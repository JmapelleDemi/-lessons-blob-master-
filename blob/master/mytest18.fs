// 47.4.1
let f n = 
  if n = 0 then 1 else
  let mutable z = 1
  let mutable step = 1
  while step <= n do
    z    <- z * step
    step <- step + 1
  z
