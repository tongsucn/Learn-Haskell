module H99_12
where

data Duplicate a = Multiple Int a | Single a deriving Show

decodeModified :: [Duplicate Char] -> String
decodeModified [] = ""
decodeModified (ele:lstTail) = case ele of {
  Single char -> char : (decodeModified lstTail);
  Multiple num char -> (replicate num char) ++ (decodeModified lstTail);
}
