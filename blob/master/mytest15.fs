// 41.4.1
let list_filter f xs =
    let folder x acc =
        if f x = true then x :: acc
        else acc
    List.foldBack folder xs []

// 41.4.2
let sum (p, xs) = 
    let fb_pred x y  =
      if p(x) = true && p(y) = true then x+y
      elif p(x) = true && p(y) = false then x
      elif p(x) = false && p(y) = true then y
      else 0
    List.foldBack fb_pred xs 0

// 41.4.3
let revrev xs = 
  let rev lst = List.fold (fun head tail -> tail::head) [] lst
  let folder acc x = 
    let reverse_el = rev x
    reverse_el::acc
  List.fold folder [] xs
