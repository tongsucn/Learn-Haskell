module H99_24
where

import Data.List
import System.Random
import Data.Time.Clock

diff_select :: Int -> Int -> IO [Int]
diff_select num upBound
  | num > upBound = error "Out of range!"
  | num <= 0 = return []
  | otherwise = do
    -- Generating random seed according to current time
    currTime <- getCurrentTime
    let randSeed = floor $ utctDayTime currTime
    -- Creating random generator
    let randGen = mkStdGen $ randSeed
    -- Generating list without redundancy
    return $ take num $ nub $ randomRs (1, upBound) randGen
