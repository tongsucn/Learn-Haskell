module H99_49
where

gray :: Int -> [String]
gray num
  | num <= 0 = [[]]
  | otherwise = gen '0' ++ (gen '1')
    where
      gen first = map (\x -> first:x) (gray (num - 1))
