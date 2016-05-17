module H99_13
where

data Duplicate a = Multiple Int a | Single a deriving Show

encodeDirect :: String -> [Duplicate Char]
encodeDirect "" = []
encodeDirect (ele:lstTail)
  | repeatLen > 1 = (Multiple repeatLen ele) : (encodeDirect rest)
  | otherwise = (Single ele) : (encodeDirect rest)
      where (first, rest) = span (== ele) lstTail
            repeatLen = length (ele:first)
