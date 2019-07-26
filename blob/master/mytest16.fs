// 42.3
let rec allSubsets n k = 
    match (n, k) with
    | (a, _) when a < 0 -> set []
    | (_, b) when b < 0 -> set []
    | (a, b) when a < b -> set []
    | (_, b) when b = 0 -> set [set []]
    | (_, _) -> Set.union (allSubsets (n-1) k) (Set.map (fun x -> Set.union x (set [n])) (allSubsets (n-1) (k-1)))
