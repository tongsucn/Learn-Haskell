module H99_06
where

isPalindrome :: String -> Bool
isPalindrome lst | null lst || length lst == 1 = True
                 | (head lst) /= (last lst) = False
                 | otherwise = isPalindrome $ tail $ init lst


