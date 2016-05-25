module H99_33
where

coprime :: Int -> Int -> Bool
coprime x y
  | x < 0 || y < 0 = coprime (abs x) (abs y)
  | x == 1 || y == 1 = True
  | x == y = False
  | otherwise = coprime (abs $ x - y) (min x y)
