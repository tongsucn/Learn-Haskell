module H99_21
where

insertAt :: a -> [a] -> Int -> [a]
insertAt item lst pos
  | null lst = [item]
  | pos < 1 = item : lst
  | pos > (length lst) = lst ++ [item]
  | otherwise = (take (pos - 1) lst) ++ [item] ++ (drop (pos - 1) lst)
