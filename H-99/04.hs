module H99_04
where

myLength :: [a] -> Int
myLength [] = 0
myLength (_:rest) = myLength rest + 1
