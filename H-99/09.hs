module H99_09
where

pack :: [Char] -> [String]
pack [] = []
pack [x] = [[x]]
pack (ele:lstTail) = (ele:first) : pack rest
      where (first, rest) = span (== ele) lstTail
