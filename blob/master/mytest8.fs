let curry f = 
    let g x = h
        let h y = f (x,y)
        h
    g
    
let uncurry g = 
    let f (x,y) = h y
        let h = g x
        h
    f
