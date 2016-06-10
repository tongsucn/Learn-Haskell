module H99_71
where

data Tree a = Node a [Tree a] deriving (Eq, Show)

tree1 :: Tree Char
tree1 = Node 'a' []
tree2 :: Tree Char
tree2 = Node 'a' [Node 'b' []]
tree3 :: Tree Char
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 :: Tree Char
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 :: Tree Char
tree5 = Node 'a' [
  Node 'f' [Node 'g' []],
  Node 'c' [],
  Node 'b' [Node 'd' [], Node 'e' []]
  ]

ipl :: Tree a -> Int
ipl (Node _ []) = 0
ipl (Node _ lst) = length lst + (sum $ map ipl lst) * 2
