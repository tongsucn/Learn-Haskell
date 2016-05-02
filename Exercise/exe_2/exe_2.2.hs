module Exe_2_2
where


-- Exercise 2.2.a
data Nats = Zero | Succ Nats deriving Show

instance Eq Nats where
  (==) Zero Zero = True
  (==) (Succ first) (Succ second) = first == second
  (==) _ _ = False


-- Exercise 2.2.b
class (Eq a) => Ordered a where
  lt, gt :: a -> a -> Bool
  gt x y = (x /= y) && (not (lt x y))


-- Exercise 2.2.c
instance Ordered Nats where
  lt _ Zero = False
  lt Zero _ = True
  lt (Succ first) (Succ second) = lt first second

instance Ordered Integer where
  lt x y = x < y


-- Exercise 2.2.d
sortAsc :: (Ordered a) => [a] -> [a]
sortAsc [] = []
sortAsc (first : rest)
  = (sortAsc (filter (\x -> (lt x first)) rest))
  ++ [first]
  ++ (sortAsc (filter (\x -> (gt x first) || (x == first)) rest))

