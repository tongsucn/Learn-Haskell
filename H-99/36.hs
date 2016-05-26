module H99_36
where

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult inputNumber
  | inputNumber < 1 = error "Input is too small!"
  | inputNumber == 1 = []
  | otherwise = (head selected, len) : (primeFactorsMult $ div inputNumber prod)
  where
    candLst = primeFactors inputNumber
    (selected, _) = span (== (head candLst)) candLst
    prod = foldl (\x y -> x * y) 1 selected
    len = length selected

primeFactors :: Int -> [Int]
primeFactors inputNumber
  | inputNumber < 1 = error "Input is too small!"
  | inputNumber == 1 = []
  | otherwise = smallestPrime : (primeFactors $ div inputNumber smallestPrime)
    where
      checkMod = \input -> mod inputNumber input == 0
      smallestPrime = head $ filter checkMod [2..inputNumber]
