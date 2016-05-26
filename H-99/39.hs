module H99_39
where

primesR :: Int -> Int -> [Int]
primesR x y
  | x < 2 || y < 2 = error "Input is too small1"
  | x > y = error "First parameter must be smaller or equal to the second one!"
  | otherwise = [selected | selected <- [x..y], isPrime selected]
    where
      isPrime selected = null $ filter (checkMod selected) [2..(selected - 1)]
      checkMod selected divisor = mod selected divisor == 0
