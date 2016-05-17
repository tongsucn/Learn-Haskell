module H99_08
where

compress :: String -> String
compress "" = ""
compress [x] = [x]
compress (ele1:ele2:rest) | ele1 == ele2 = compress (ele1:rest)
                         | otherwise = [ele1] ++ (compress (ele2:rest))
