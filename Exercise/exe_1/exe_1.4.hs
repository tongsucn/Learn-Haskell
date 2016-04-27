module Exe_1_4
where

(*+) :: [Int] -> [Int] -> Int
infixl 7 *+
(*+) [] [] = 0
(*+) (x:xs) (y:ys) = (myMult x y) + xs *+ ys

myMult :: Int -> Int -> Int
myMult first second | second == 0 = 0
                    | second > 0 = first + (myMult first (second - 1))
                    | second < 0 = -first + (myMult first (second + 1))
