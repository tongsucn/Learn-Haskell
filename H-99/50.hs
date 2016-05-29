module H99_50
where

-- Data constructor of Huffman Tree
data HTree = Node Int HTree HTree | Leaf Int Char deriving Show

huffman :: [(Char, Int)] -> [(Char, String)]
huffman inputLst
  | null inputLst = []
  | otherwise = hTree2List hTree
    where
      candTree = map (\(char, count) -> Leaf count char) inputLst
      hTree = createHTree candTree

      -- Function for converting a Huffman Tree into a list
      hTree2List :: HTree -> [(Char, String)]
      hTree2List (Leaf _ char) = [(char, "")]
      hTree2List (Node _ left right) = rResult ++ lResult
        where
          lResult = map (encode '1') $ hTree2List left
          rResult = map (encode '0') $ hTree2List right
          encode :: Char -> (Char, String) -> (Char, String)
          encode prefix (char, binStr) = (char, prefix:binStr)


-- Function for creating a Huffman Tree
createHTree :: [HTree] -> HTree
createHTree [] = error "Empty Huffman Tree!"
createHTree [root] = root
createHTree lst = createHTree $ mergeNodes larger smaller restLst
  where
    -- Find the order of the first two elements
    (larger, smaller) = if (getCount $ lst !! 0) < (getCount $ lst !! 1)
      then ((lst !! 1), (lst !! 0))
      else ((lst !! 0), (lst !! 1))
    -- Create the rest list
    restLst = drop 2 lst

    -- Function for merging the smallest two tree nodes in a list
    mergeNodes :: HTree -> HTree -> [HTree] -> [HTree]
    mergeNodes l s (ele:rest)
      | lCount <= headCount = ele : (mergeNodes l s rest)
      | lCount > headCount && sCount < headCount = l : (mergeNodes ele s rest)
      | otherwise = l : (mergeNodes s ele rest)
      where
        lCount = getCount l
        sCount = getCount s
        headCount = getCount ele
    mergeNodes left right [] = [Node (lCount + rCount) left right]
      where
        lCount = getCount left
        rCount = getCount right

    -- Function for getting node's count
    getCount :: HTree -> Int
    getCount (Node count _ _) = count
    getCount (Leaf count _) = count
