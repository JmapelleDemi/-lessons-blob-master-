// 40.1 
let rec sum (p, xs) =
 match (p,xs) with
 | (p,[])         -> 0
 | (p,head::tail) -> if p head = true then head + (sum (p,tail)) else sum (p,tail) 

// 40.2.1
let rec count (xs, n) = 
  match (xs,n) with
  | ([],n)                        -> 0
  | (head::tail, n) when head > n -> 0
  | (head::tail, n)               -> if n = head then 1 + count (tail, n) else count (tail, n)

// 40.2.2
let rec insert (xs, n) =
  match (xs,n) with
  | ([], n)         -> [n]
  | (head::tail, n) -> if head >= n then n :: head :: tail else head :: insert (tail,n)

// 40.2.3
let rec intersect (xs1, xs2) =
  match (xs1, xs2) with
  | ([],[]) | ([],_) | (_,[]) -> []
  | (xs1, xs2) -> 
    let rec tolist (zzz,list) =
      match (zzz,list) with
      | (zzz,head::list) when zzz = head -> [zzz]
      | (zzz,head::list)                 -> tolist (zzz,list)
      | _                                -> []
    let head::tail = xs1  
    tolist (head,xs2) @ intersect(tail,xs2)

// 40.2.4
let rec plus (xs1, xs2) =
 match (xs1, xs2) with
  | ([],[]) | (_,[]) | ([],_)   -> xs1 @ xs2
  | (head1::tail1,head2::tail2) -> if head2 >= head1 then plus (tail1,head1::head2::tail2) else head2 :: plus (head1::tail1,tail2)

// 40.2.5
let rec minus (xs1, xs2) = 
  match (xs1, xs2) with
  | (xs1, [])                   -> xs1
  | ([], xs2)                   -> []
  | ([],[])                     -> []
  | (head1::tail1,head2::tail2) -> if head1 = head2 then minus (tail1,tail2) else head1 :: minus (tail1,head2::tail2)

// 40.3.1
let rec smallest = function
  | [] -> None
  | [x] -> Some x
  | head1::(head2::tail) when head1 <= head2 -> smallest(head1::tail)
  | head1::(head2::tail)                     -> smallest(head2::tail)

// 40.3.2
let rec delete (n, xs) = 
  match (n, xs) with
  | (n,[])         -> []
  | (n,head::tail) -> if n = head then tail  else head :: delete (n,tail)

// 40.3.3
let rec sort = function
  | [] -> []
  | x -> 
    let head = smallest(x)
    [Option.get <| head] @ sort(delete(Option.get(head), x))

// 40.4
let rec revrev = 
    let rec rev = function
        | head :: tail -> List.rev head :: rev tail
        | _ -> []
    fun xs -> rev (List.rev xs)
