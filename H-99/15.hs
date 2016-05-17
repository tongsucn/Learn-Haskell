module H99_15
where

repli :: String -> Int -> String
repli "" _ = ""
repli _ 0 = ""
repli (ele:rest) num | num <= 0 = ""
                     | otherwise
            = (ele : (repli [ele] (num - 1))) ++ (repli rest num)
