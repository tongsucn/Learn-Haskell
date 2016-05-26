module H99_40
where

goldbach :: Int -> (Int, Int)
goldbach inputNumber
  | inputNumber <= 2 = error "Input must be larger than 2!"
  | otherwise = (firstPrime, inputNumber - firstPrime)
    where
      primesR = \x y -> [selected | selected <- [x..y], isPrime selected]
        where
          isPrime input = null $ filter (checkMod input) [2..(input - 1)]
          checkMod input divisor = mod input divisor == 0
      candLst = primesR 2 inputNumber
      checkGoldbach = \x -> elem (inputNumber - x) candLst
      firstPrime = head $ filter checkGoldbach candLst
