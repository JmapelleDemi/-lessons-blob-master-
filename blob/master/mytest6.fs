// 17.1 
let rec pow = function
 | (s,0) -> ""
 | (s,n) -> s + pow (s,n-1) 
 
// 17.2
let rec isIthChar = function
 | (s,z,c) when z >= String.length s -> false
 | (s,n,c) -> (string(s)).[n] = c
 
// 17.3
let rec occFromIth = fun (s,n,c) -> 
  let bti = function
   | true  ->  1
   | false ->  0
  match (s,n,c) with
   | (s,l,c) when l = String.length s -> 0
   | (s,q,c) when q > String.length s -> 0
   | (s,n,c)  -> bti((string(s)).[n] = c) + occFromIth (s,n+1,c) 
