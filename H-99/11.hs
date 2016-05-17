module H99_11
where

data Duplicate a = Multiple Int a | Single a deriving Show

encodeModified :: String -> [Duplicate Char]
encodeModified "" = []
encodeModified (ele:lstTail) | null first = (Single ele) : (encodeModified rest)
                             | otherwise = (Multiple (length (ele:first)) ele)
                                            : (encodeModified rest)
              where (first, rest) = span (== ele) lstTail
