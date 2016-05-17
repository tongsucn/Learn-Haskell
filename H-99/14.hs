module H99_14
where

dupli :: [a] -> [a]
dupli [] = []
dupli (ele:rest) = ele : ele : (dupli rest)
