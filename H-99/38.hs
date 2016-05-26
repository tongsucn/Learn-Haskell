module H99_38
where

import Data.Time.Clock

main :: IO ()
main = do
  let targetNum = 10090
  putStr "Target number: "
  print targetNum

  -- phi method
  putStrLn "Phi method:"
  tikPhi <- getCurrentTime
  resultPhi <- phiIO targetNum
  putStr "Result: "
  print resultPhi
  tokPhi <- getCurrentTime
  putStr "Time: "
  print $ utctDayTime tokPhi - (utctDayTime tikPhi)

  -- totient method
  putStrLn "Totient method:"
  tikTot <- getCurrentTime
  resultTot <- totientIO targetNum
  putStr "Result: "
  print resultTot
  tokTot <- getCurrentTime
  putStr "Time: "
  print $ utctDayTime tokTot - (utctDayTime tikTot)
  return ()

phiIO :: Int -> IO Int
phiIO target = return $ phi target

totientIO :: Int -> IO Int
totientIO target = return $ totient target

-- Naive algorithms
phi :: Int -> Int
phi x
  | x <= 0 = error "Input is too small!"
  | otherwise = length $ filter (coprime x) [1..x]

coprime :: Int -> Int -> Bool
coprime x y
  | x < 0 || y < 0 = coprime (abs x) (abs y)
  | x == 1 || y == 1 = True
  | x == y = False
  | otherwise = coprime (abs $ x - y) (min x y)

-- More efficient algorithms
totient :: Int -> Int
totient inputNumber = foldl computeTot 1 lstForm
  where
    lstForm = primeFactorsMult inputNumber
    computeTot prevResult (prime, num)
      = prevResult * (prime - 1) * (prime ^ (num - 1))

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
