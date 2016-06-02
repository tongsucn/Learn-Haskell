module H99_59
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

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
