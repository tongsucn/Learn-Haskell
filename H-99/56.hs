module H99_56
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symmetric :: Tree Char -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right

mirror :: Tree Char -> Tree Char -> Bool
mirror Empty Empty = True
mirror (Branch _ lleft lright) (Branch _ rleft rright)
  = mirror lleft rright && (mirror lright rleft)
mirror _ _ = False
