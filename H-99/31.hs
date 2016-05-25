module H99_31
where

isPrime :: Int -> Bool
isPrime val
  | val < 2 = error "The input is too small (< 2)!"
  | otherwise = null $ filter checkMod [2..(val - 1)]
    where
      checkMod divisor = mod val divisor == 0
