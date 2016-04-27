module Exe_2_1
where


-- Exercise 2.1.a
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf

tree1 :: BinaryTree Char
tree1 = Node 'g' (Node 'u' (Node 'l' Leaf Leaf) (Node 'f' Leaf Leaf))
  (Node '2' (Node 'i' Leaf Leaf) Leaf)

tree2 :: BinaryTree Int
tree2 = Node 4 (Node 2 (Node 1 Leaf Leaf) Leaf)
  (Node 10 (Node 6 Leaf Leaf) (Node 42 Leaf Leaf))


-- Exercise 2.1.b
flattenTree :: BinaryTree a -> [a]
flattenTree Leaf = []
flattenTree (Node node left right)
  = ((flattenTree left) ++ [node]) ++ (flattenTree right)


-- Exercise 2.1.c
elemTree :: Eq a => a -> BinaryTree a -> Bool
elemTree _ Leaf = False
elemTree input (Node node left right)
  = (node == input)
  || (elemTree input left)
  || (elemTree input right)


-- Exercise 2.1.d
checkLeftValue :: Ord a => a -> BinaryTree a -> Bool
checkLeftValue _ Leaf = True
checkLeftValue input (Node node left right)
  = (input > node)
  && (checkLeftValue input left)
  && (checkLeftValue input right)

checkRightValue :: Ord a => a -> BinaryTree a -> Bool
checkRightValue _ Leaf = True
checkRightValue input (Node node left right)
  = (input < node)
  && (checkRightValue input left)
  && (checkRightValue input right)

isSorted :: Ord a => BinaryTree a -> Bool
isSorted Leaf = True
isSorted (Node node left right)
  = (checkLeftValue node left)
  && (checkRightValue node right)
  && (isSorted left)
  && (isSorted right)

