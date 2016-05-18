module H99_18
where

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice lst from to
  | from < 0 || to >= (length lst) = error "Index out of range!"
  | otherwise = drop (from - 1) (take to lst)
