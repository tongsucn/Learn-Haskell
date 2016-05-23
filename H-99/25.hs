module H99_25
where

import Data.List
import System.Random
import Data.Time.Clock

rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu lst = do
    let lstLen = length lst
    -- Generating random seed according to current time
    currTime <- getCurrentTime
    let randSeed = floor $ utctDayTime currTime
    -- Creating random generator
    let randGen = mkStdGen $ randSeed
    -- Generating index list without redundancy
    let idxLst = take lstLen $ nub $ randomRs (0, lstLen - 1) randGen
    return [lst !! idx | idx <- idxLst]
