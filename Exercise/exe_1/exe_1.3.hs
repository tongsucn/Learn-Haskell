module Exe_1_3
where

-- Exercise 1.3.a
digitSum :: Int -> Int
digitSum 0 = 0
digitSum input = mod input 10 + digitSum (div input 10)

-- Exercise 1.3.b
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (item : []) = True
isSorted (first : (second : rest))
  = if first <= second then isSorted (second : rest) else False

-- Exercise 1.3.c
intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect (ele : rest) range
  = (singleCheck ele range) ++ (intersect rest range)

singleCheck :: Int -> [Int] -> [Int]
singleCheck ele [] = []
singleCheck ele (first : rest)
  = if ele == first then [ele] else singleCheck ele rest

-- Exercise 1.3.d
sortedIntersect :: [Int] -> [Int] -> [Int]
sortedIntersect [] _ = []
sortedIntersect _ [] = []
sortedIntersect (head1 : rest1) (head2 : rest2)
  | head1 == head2 = [head1] ++ (sortedIntersect rest1 rest2)
  | head1 < head2 = sortedIntersect rest1 (head2 : rest2)
  | head1 > head2 = sortedIntersect (head1 : rest1) rest2
