module H99_32
where

myGCD :: Int -> Int -> Int
myGCD x y
  | x < 0 || y < 0 = myGCD (abs x) (abs y)
  | x == 1 || y == 1 = 1
  | x == y = x
  | otherwise = myGCD (abs $ x - y) (min x y)
