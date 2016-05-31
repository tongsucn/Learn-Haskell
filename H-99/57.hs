module H99_57
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

construct :: Ord a => [a] -> Tree a
construct [] = Empty
construct (headEle:rest) = Branch headEle (construct smaller) (construct larger)
  where
    smaller = filter (< headEle) rest
    larger = filter (>= headEle) rest
