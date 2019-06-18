// 20.3.1.
let vat n x = x * ( float n * 0.01 ) + x

// 20.3.2.
let unvat n x = x * 100.0 / (100.0 + float n)

// 20.3.3.
let rec min f =
    let rec testf = function
      | n when f n = 0 -> n
      | n -> testf (n+1)
    testf 0
