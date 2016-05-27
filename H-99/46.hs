module H99_46
where

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' x y
  | x == y = False
  | otherwise = True

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' x y
  | x == y = True
  | otherwise = False

table :: (Bool -> Bool -> Bool) -> IO ()
table func = do
  let inputLst = [True, False]
  let candLst = [(first, second) | first <- inputLst, second <- inputLst]
  let resultLst = map (\(x, y) -> (x, y, func x y)) candLst
  outputResult resultLst
  return ()

outputResult :: [(Bool, Bool, Bool)] -> IO ()
outputResult [] = return ()
outputResult ((x, y, z):rest)
  = putStr (show x) >> putStr " " >> putStr (show y) >> putStr " "
    >> putStrLn (show z) >> outputResult rest
