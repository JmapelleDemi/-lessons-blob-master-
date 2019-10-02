// 47.4.1
let f n = 
  if n = 0 then 1 else
  let mutable z = 1
  let mutable step = 1
  while step <= n do
    z    <- z * step
    step <- step + 1
  z

// 47.4.2
let fibo n =
  if n = 0 then 0 
  elif n = 1 then 1 else
  let mutable first = 0
  let mutable second = 1
  let mutable step = 1
  let mutable sum = first + second
  while step < n do
    sum     <- first + second
    first   <- second
    second  <- sum
    step    <- step + 1
  sum
