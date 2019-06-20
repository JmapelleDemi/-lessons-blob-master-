// 34.1
let rec upto = fun n -> [1..n]

// 34.2
let rec dnto = fun n -> if n = 0 then [] else n :: dnto(n-1)

// 34.3
let evenn =
    let rec iter_evenn (n, i) =
        if n = 1 then [ i ]
        else i :: iter_evenn (n - 1, i + 2)
    function
 | n when n >= 1 -> iter_evenn (n, 0)
 | _ -> []
