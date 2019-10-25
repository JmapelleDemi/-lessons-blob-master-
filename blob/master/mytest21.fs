// 50.2.1
let fac_seq = seq {
    for i in 1..n do 
      let factorial n = 
         let rec f x a = 
             if x <= 1 then a 
             else f (x - 1) (a * x) 
         f n 1 
      yield i }
      
// 50.2.2
let seq_seq = seq {
    for i in 1..n do 
      if i % 2 = 0 then  yield (i / 2)
      else yield ( - (i + 1) / 2) }
