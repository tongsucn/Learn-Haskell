module H99_34
where

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
