module Exe_2_3
where


import Data.Char


-- Exercise 2.3.a
length' :: [a] -> Int
length' ls = foldr (\_ y -> y + 1) 0 ls


-- Exercise 2.3.b
countLetters :: [Char] -> Int
countLetters str = length' (filter isLetter str)
