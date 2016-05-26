module H99_35
where

primeFactors :: Int -> [Int]
primeFactors inputNumber
  | inputNumber < 1 = error "Input is too small!"
  | inputNumber == 1 = []
  | otherwise = smallestPrime : (primeFactors $ div inputNumber smallestPrime)
    where
      checkMod = \input -> mod inputNumber input == 0
      smallestPrime = head $ filter checkMod [2..inputNumber]
