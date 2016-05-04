module Exe_3_1
(
fibs,
fib,
rands,
randSum
)
where


-- Exe 3.1.a
fibs :: [Int]
fibs = 1 : 1 : (zipWith (\x y -> x + y) fibs (tail fibs))

fib :: Int -> Int
fib x | x < 0 = 0
      | otherwise = fibs !! x

-- Exe 3.1.b
rands :: [Int]
rands = 42 : (map (\x -> mod (25713 * x + 13849) (2 ^ 16)) rands)

randSum :: Int -> Int
randSum x | x <= 0 = 0
          | otherwise = sum $ take x rands
