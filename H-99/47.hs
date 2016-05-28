module H99_47
where

and' :: Bool -> Bool -> Bool
infixl 7 `and'`
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
infixl 7 `or'`
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
infixl 7 `nand'`
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
infixl 7 `nor'`
nor' False False = True
nor' _ _ = False

xor' :: Bool -> Bool -> Bool
infixl 7 `xor'`
xor' x y
  | x == y = False
  | otherwise = True

impl' :: Bool -> Bool -> Bool
infixl 6 `impl'`
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
infixl 6 `equ'`
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
