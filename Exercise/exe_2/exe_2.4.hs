module Exe_2_4
where


-- Exercise 2.4.a
data MultTree a = MultNode a [MultTree a] deriving Show

mtree1 :: MultTree Int
mtree1
  = MultNode 8
  [MultNode 3 [MultNode (-56) [], MultNode 4 [], MultNode 987 []],
  MultNode 4 [MultNode 6 []]]

mtree2 :: MultTree Int
mtree2
  = MultNode (-2)
  [MultNode 5 [MultNode 16 [], MultNode 7 []],
  MultNode (-9) [MultNode 1 [], MultNode 5 []]]


zipWithMult :: (a -> b -> c) -> MultTree a -> MultTree b -> MultTree c
zipWithMult func (MultNode first []) (MultNode second _)
  = MultNode (func first second) []
zipWithMult func (MultNode first _) (MultNode second [])
  = MultNode (func first second) []
zipWithMult func (MultNode first firstLst) (MultNode second secondLst)
  = MultNode (func first second)
  (zipWith (\x y -> zipWithMult func x y) firstLst secondLst)
