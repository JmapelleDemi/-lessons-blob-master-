// 43.3
let try_find key m = 
  if (Map.containsKey key m) = true then Some(Map.find key m) else None
