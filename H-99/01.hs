module H99_01
where

myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast lst = lst !! (length lst - 1)

myLast' :: [a] -> a
myLast' [] = error "Empty list!"
myLast' (ele:[]) = ele
myLast' (_:rest) = myLast' rest
