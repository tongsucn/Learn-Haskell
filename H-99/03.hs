module H99_03
where

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list!"
elementAt lst 1 = head lst
elementAt (ele:rest) idx | idx < 1 || length (ele:rest) < idx
                                     = error "Index out of range!"
                         | otherwise = elementAt rest (idx - 1)

