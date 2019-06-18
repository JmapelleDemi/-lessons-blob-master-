// 23.4.1
let (.+.) x y = 
  let (a:int,b:int,c:int) = x 
  let (d:int,e:int,f:int) = y
  let ad = a+d
  let be = b+e
  let cf = c+f
  (ad + int (float be / 20.0) , int ((float be) % 20.0) + int (float cf / 12.0) , int ((float cf) % 12.0))

let (.-.) x y = 
  let (a:int,b:int,c:int) = x 
  let (d:int,e:int,f:int) = y
  let ad = a-d
  let be = b-e
  let cf = c-f
  let rec check1 = function
   | (aa,bb,cc) when bb < 0 &&  cc < 0 -> check1 (aa-1 , bb+19 , cc+12)
   | (aa,bb,cc) when cc < 0            -> check1 (aa   , bb-1  , cc+12)
   | (aa,bb,cc) when bb < 0            -> check1 (aa-1 , bb+20 , cc   )
   | (aa,bb,cc)                        -> (aa,bb,cc) 
  check1 (ad,be,cf)
  
// 23.4.2
let (.+) x y = 
  let (a:float,b:float) = x
  let (c:float,d:float) = y
  (a+c,b+d)

let (.-) x y =
  let (a:float,b:float) = x
  let (c:float,d:float) = y
  (a-c,b-d)

let (.*) x y = 
  let (a:float,b:float) = x
  let (c:float,d:float) = y
  (a*c - b*d, b*c + a*d)
  
let (./) x y = 
  let (a:float,b:float) = x
  let (c:float,d:float) = y
  (a,b) .* (c/(c*c+d*d),-d/(c*c+d*d))
