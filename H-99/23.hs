module H99_23
where

import System.Random
import Data.Time.Clock

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
