module H99_55
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

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
