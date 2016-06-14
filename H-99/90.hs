module H99_90
where

queens :: Int -> [[Int]]
queens num = solveQueens []
  where
    solveQueens ans
      | length ans == num = [ans]
      | otherwise = if null candLst then [] else res
        where
          candLst = filter (checkValid ans) [1..num]
          nestedRes = map (\x -> solveQueens (ans ++ [x])) candLst
          res = foldl (\x y -> x ++ y) [] nestedRes

checkValid :: [Int] -> Int -> Bool
checkValid lst num = null conflictLst
  where
    conflictIdx dist rowIdx = [rowIdx - dist, rowIdx, rowIdx + dist]
    checkExist idx = elem (lst !! idx) (conflictIdx (length lst - idx) num)
    conflictLst = [idx | idx <- [0..(length lst - 1)], checkExist idx]
