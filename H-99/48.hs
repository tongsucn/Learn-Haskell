module H99_48
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

table' :: Int -> ([Bool] -> Bool) -> IO ()
table' num func = do
  let candLst = genBool num
  let outLst = zipWith (\x y -> x ++ [y]) candLst (map func candLst)
  genOutput outLst
  return ()

genBool :: Int -> [[Bool]]
genBool num
  | num <= 0 = [[]]
  | otherwise = gen True ++ (gen False)
    where
      gen first = map (\x -> first:x) (genBool (num - 1))

genOutput :: [[Bool]] -> IO ()
genOutput outLst = do
  let toStr = foldr (\x y -> (show x) ++ (if x then "  " else " ") ++ y) ""
  let outStrLst = map toStr outLst
  putStr $ foldr (\ln resStr -> init ln ++ "\n" ++ resStr) "" outStrLst
  return ()
