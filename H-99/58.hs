module H99_58
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symCbalTrees :: Int -> [Tree Char]
symCbalTrees num
  | num <= 0 = error "Input is too small!"
  | num == 1 = [Branch 'x' Empty Empty]
  | (rem num 2) == 0 = error "Please enter an odd!"
  | otherwise = [Branch 'x' child (mirrorTree child) | child <- (cbalTree cNum)]
    where
      cNum = div (num - 1) 2

mirrorTree :: Tree Char -> Tree Char
mirrorTree Empty = Empty
mirrorTree (Branch val left right) = Branch val mLeft mRight
  where
    mRight = mirrorTree left
    mLeft = mirrorTree right

cbalTree :: Int -> [Tree Char]
cbalTree num
  | num < 0 = error "The number of nodes is too small!"
  | num == 0 = [Empty]
  | num == 1 = [Branch 'x' Empty Empty]
  | otherwise = if lNum == rNum
      then [Branch 'x' l r | l <- lBranch, r <- rBranch]
      else [Branch 'x' l r | l <- lBranch, r <- rBranch]
            ++ [Branch 'x' l r| l <- rBranch, r <- lBranch]
    where
      lNum = div (num - 1) 2
      rNum = num - 1 - lNum
      lBranch = cbalTree lNum
      rBranch = cbalTree rNum
