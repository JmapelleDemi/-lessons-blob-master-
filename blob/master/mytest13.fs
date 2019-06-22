// 39.1
let rec rmodd = fun n ->  
  let rec puteven z =
    match z with
     | []           -> []
     | [z]          -> [z]
     | head :: tail -> head :: rmodd tail
  match n with
  | []              -> []
  | [x]             -> []
  | head :: tail    ->  puteven tail

// 39.2
let rec del_even = fun l ->
  let odd = fun number   ->
    if number % 2 = 1 then true else false
  match l with
  | []           -> []
  | head :: tail -> if odd head = true then head :: del_even tail else del_even tail

// 39.3
let rec multiplicity x xs =
 match (x,xs) with
  | (_,[])           -> 0
  | (x,head :: tail) -> if x = head then 1 + multiplicity x tail else multiplicity x tail
  
// 39.4
let rec split = fun x ->
  let rec ll = fun x -> 
    let rec putodd z =
      match z with
       | []  -> []
       | [z] -> []
       | head :: tail -> ll tail 
    match x with
    | []  -> []
    | [x] -> [x]
    | head :: tail   ->  head :: putodd tail
  let rec rr = fun x ->   //выдаёт список, в который входят значения входного списка на нечётных позициях
    let rec puteven z =
      match z with
       | []  -> []
       | [z] -> [z]
       | head :: tail -> head :: rr tail
    match x with
    | []  -> []
    | [x] -> []
    | head :: tail   ->  puteven tail
  (ll x,rr x)
  
// 39.5
let rec zip (xs1,xs2) =
  if List.length xs1 <> List.length xs2
  then failwith "Uncorrect length of lists"
  else
  match (xs1,xs2) with
  | ([],[])                     -> []
  | (_,[]) | ([],_)             -> []
  | (headx::tailx,heady::taily) -> (headx,heady) :: zip (tailx,taily)
