module H99_05
where

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (ele:rest) = myReverse rest ++ [ele]
