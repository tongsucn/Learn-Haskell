module H99_19
where

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate lst num
  | num == 0 = lst
  | num > 0 = (drop num lst) ++ (take num lst)
  | num < 0 = (drop (num + (length lst)) lst) ++ (take (num + (length lst)) lst)
