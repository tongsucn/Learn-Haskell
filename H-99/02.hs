module H99_02
where

myButLast :: [a] -> a
myButLast lst | length lst < 2 = error "List is too short!"
              | otherwise = lst !! (length lst - 2)

