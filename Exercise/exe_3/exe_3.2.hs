module Exe_3_2
where

-- Exe 3.2.a
drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [y | y <- xs, y `mod` x /= 0]

dropall :: [Int] -> [Int]
dropall (x : xs) = x : dropall (drop_mult x xs)

primes :: [Int]
primes = dropall [2..]

primeFactors :: Int -> [Int]
primeFactors x = [y | y <- (take x primes), (mod x y) == 0]


-- Exe 3.2.b
merge :: Ord a => [a] -> [a] -> [a]
merge (x : xs) (y : ys) | x < y = x : (merge xs (y : ys))
                        | otherwise = y : (merge (x : xs) ys)
merge xs ys = xs ++ ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge
      (mergeSort (splitList True xs))
      (mergeSort (splitList False xs))

splitList :: Bool -> [a] -> [a]
splitList part lst = [lst !! idx
  | idx <- if part
  then [0..(div (length lst) 2 - 1)]
  else [(div (length lst) 2)..(length lst - 1)]]
