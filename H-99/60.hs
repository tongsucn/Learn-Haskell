module H99_60
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes ele num
  | num < 0 = error "Input is too small!"
  | num == 0 = [Empty]
  | num == 1 = [Branch ele Empty Empty]
  | otherwise = filter (\tree -> countNode tree == num) (hbalTree ele height)
    where
      height = div num 2 + 1

countNode :: Tree a -> Int
countNode Empty = 0
countNode (Branch _ lTree rTree) = 1 + (countNode lTree) + (countNode rTree)

hbalTree :: a -> Int -> [Tree a]
hbalTree ele height
  | height < 0 = error "Input is too small!"
  | height == 0 = [Empty]
  | height == 1 = [Branch ele Empty Empty]
  | otherwise = [Branch ele lTree rTree | lTree <- subTrees, rTree <- subTrees]
    where
      subTree1 = hbalTree ele (height - 1)
      subTree2 = hbalTree ele (height - 2)
      subTrees = subTree1 ++ subTree2
