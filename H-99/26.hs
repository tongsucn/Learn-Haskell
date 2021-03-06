module H99_26
where

combinations :: Int -> [a] -> [[a]]
combinations num lst
  | num < 0 || num > (length lst) = error "Out of range!"
  | num == 0 = [[]]
  | (length lst) == 0 = [[]]
  | otherwise = concat [map (joinHead idx) (genTail idx) | idx <- candIdxLst]
    where
      joinHead idx tailLst = (lst !! idx) : tailLst
      genTail idx = combinations (num - 1) (drop (idx + 1) lst)
      candIdxLst = [0..(length lst - num)]

