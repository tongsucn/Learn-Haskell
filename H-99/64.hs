module H99_64
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree64 :: Tree Char
tree64 = Branch 'n'
  (Branch 'k'
    (Branch 'c'
      (Branch 'a' Empty Empty)
      (Branch 'h'
        (Branch 'g'
          (Branch 'e' Empty Empty)
          Empty)
        Empty))
    (Branch 'm' Empty Empty))
  (Branch 'u'
    (Branch 'p'
      Empty
      (Branch 's'
        (Branch 'q' Empty Empty)
        Empty))
    Empty)

layout :: Tree a -> Tree (a, (Int, Int))
layout Empty = Empty
layout tree = layoutCalc tree 1

layoutCalc :: Tree a -> Int -> Tree (a, (Int, Int))
layoutCalc Empty _ = Empty
layoutCalc (Branch ele lTree rTree) level
  = Branch (ele, (newX, level)) newLeftTree newRightTree
    where
      newX = countNodes lTree + 1
      newLevel = level + 1
      newLeftTree = layoutCalc lTree newLevel
      newRightTree = rightCalc rTree newX newLevel

rightCalc :: Tree a -> Int -> Int -> Tree (a, (Int, Int))
rightCalc Empty _ _ = Empty
rightCalc (Branch ele lTree rTree) x level
  = Branch (ele, (newX, level)) newLeftTree newRightTree
    where
      newX = countNodes lTree + x + 1
      newLevel = level + 1
      newLeftTree = rightCalc lTree x newLevel
      newRightTree = rightCalc rTree newX newLevel

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ lTree rTree) = 1 + (countNodes lTree) + (countNodes rTree)
