module H99_20
where

removeAt :: Int -> [a] -> (a, [a])
removeAt idx lst
  | (length lst) < idx = error "Index out of range!"
  | otherwise = (lst !! (idx - 1), (take (idx - 1) lst) ++ (drop idx lst))
