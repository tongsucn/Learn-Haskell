module H99_62B
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch ele lTree rTree) level
  | level == 1 = [ele]
  | otherwise = atLevel lTree nextLevel ++ (atLevel rTree nextLevel)
    where nextLevel = level - 1
