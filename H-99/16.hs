module H99_16
where

dropEvery :: String -> Int -> String
dropEvery "" _ = ""
dropEvery lst num
  | num <= 1 = ""
  | num > (length lst) = lst
  | otherwise = (take (num - 1) lst) ++ (dropEvery (drop num lst) num)
