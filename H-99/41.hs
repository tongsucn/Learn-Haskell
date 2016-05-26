module H99_41
where

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y
  | x > y = error "First parameter must be smaller or equal to the second one!"
  | x < 3 = goldbachList 3 y
  | otherwise = map goldbach targetLst
    where
      targetLst = [target | target <- [x..y], mod target 2 == 0]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' x y th = filter thFilter allResults
  where
    allResults = goldbachList x y
    thFilter = \(first, _) -> first > th

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
