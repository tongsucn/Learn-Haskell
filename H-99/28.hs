module H99_28
where

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort lst = (lfsort smallerSubLst) ++ [headEle] ++ (lfsort biggerSubLst)
  where
    headEle = head lst
    tails = tail lst
    smallerSubLst = filter (\ele -> length ele < (length headEle)) tails
    biggerSubLst = filter (\ele -> length ele >= (length headEle)) tails
