module H99_07
where

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (ele:rest)) = (flatten ele) ++ (flatten (List rest))
