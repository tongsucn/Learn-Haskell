module H99_24
where

import System.Random
import Data.Time.Clock

diff_select :: Int -> Int -> IO [Int]
diff_select num upBound
  | num > upBound = error "Out of range!"
  | num <= 0 = return []
  | otherwise = do
    let rangeLst = [1..upBound]

    return ((rangeLst !! idx) : diff_select (num - 1) (take idx (drop (idx + 1) rangeLst)))

rnd_select :: [a] -> Int -> IO [a]
rnd_select lst num
  | num <= 0 = do
    return []
  | otherwise = do
    let upIdx = length lst - 1
    -- Generating random seed according to current time
    currTime <- getCurrentTime
    let randSeed = floor $ utctDayTime currTime
    -- Creating random generator and random list
    let randGen = mkStdGen $ randSeed
    let randIdxLst = take num $ randomRs (0, upIdx) randGen
    return [lst !! idx | idx <- randIdxLst]
