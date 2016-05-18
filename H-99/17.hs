module H99_17
where

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split lst num | num < 0 = ([], lst)
              | num > (length lst) = (lst, [])
              | otherwise = (take num lst, drop num lst)
