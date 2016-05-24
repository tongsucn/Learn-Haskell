module H99_27
where

import Data.List

myGroup :: Eq a => [Int] -> [a] -> [[[a]]]
myGroup pat lst
  | length (nub lst) /= (length lst) = error "Redundants exist in list!"
  | sum pat /= (length lst) = error "Pattern does not match list!"
  | not $ null $ filter (< 0) pat = error "Pattern contain negative!"
  | null lst = [[]]
  | otherwise = concat $ map appTail restLst
    where
      appTail = \headEle -> (map (joinHead headEle) (genTail (lst \\ headEle)))
        where
          joinHead headEle rest = headEle : rest
          restPat = tail pat
          genTail tailCandidate = myGroup restPat tailCandidate
      restLst = combinations (head pat) lst


combinations :: Int -> [a] -> [[a]]
combinations num lst
  | num < 0 || num > (length lst) = error "Out of range!"
  | num == 0 = [[]]
  | (length lst) == 0 = [[]]
  | otherwise = concat [map (joinHead idx) (genTail idx) | idx <- candIdxLst]
    where
      joinHead idx tailLst = (lst !! idx) : tailLst
      genTail idx = combinations (num - 1) (drop (idx + 1) lst)
      candIdxLst = [0..(length lst - num)]

