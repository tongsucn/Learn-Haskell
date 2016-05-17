module H99_10
where

encode :: String -> [(Int, Char)]
encode "" = []
encode (ele:lstTail) = (length (ele:first), ele) : encode rest
      where (first, rest) = span (== ele) lstTail
