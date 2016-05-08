import Data.Char

data CalculatorInput = Exit | Error String | Operator (Int -> Int -> Int) | Number Int

instance Show CalculatorInput where
  show Exit = "Exit"
  show (Error xs) = "Invalid Input: " ++ xs
  show (Operator _) = "Operator _"
  show (Number i) = "Number " ++ (show i)

isNumberS :: String -> Bool
isNumberS s@(x:xs) = and (map isDigit s)
isNumberS _ = False

parseCalculatorInput :: String -> CalculatorInput
parseCalculatorInput ('-':xs) | isNumberS xs = Number (-((read xs) :: Int))
parseCalculatorInput (c:[]) | c == '+' = Operator (+)
                            | c == '-' = Operator (-)
                            | c == '*' = Operator (*)
                            | c == '/' = Operator div
parseCalculatorInput (x:xs) | isSpace x = parseCalculatorInput xs
                            | isNumberS (x:xs) = Number ((read (x:xs)) :: Int)
parseCalculatorInput xs | elem (map toLower xs) ["q", "e", "exit", "quit"] = Exit
                        | otherwise = Error xs


-- exercise:

main :: IO ()
main = do
  -- task a)
  -- replace with implementation:
  -- Set initialization value and operator
  let initValue = 0
      initOp = (\_ y -> y)

  -- Print welcome information and start loop
  putStrLn "Welcome to the simple Haskell calculator!"
  putStrLn $ show initValue
  simpleCalculator initValue initOp

  -- Quit from loop and say goodbye
  putStrLn "Bye!"
  -- end replace

simpleCalculator :: Int -> (Int -> Int -> Int) -> IO ()
simpleCalculator ans op =  do
  -- task c)
  -- replace with implementation:
  -- Define a function for matching patterns of CalculatorInput data
  let
    matchInput :: CalculatorInput -> IO ()
    matchInput Exit = return ()
    matchInput (Error str) = do
              putStrLn $ "Invalid Input: " ++ str
              putStrLn $ show ans
              simpleCalculator ans op
    matchInput (Operator newOp) = simpleCalculator ans newOp
    matchInput (Number num) = do
              let newAns = op ans num
              putStrLn $ show newAns
              simpleCalculator newAns op

  -- Getting input
  newInput <- getInput
  -- Matching input and perform operations
  matchInput newInput
  -- end replace

getInput :: IO CalculatorInput
getInput = do
  -- task b)
  -- replace with implementation:
  putStr "> "
  inputStr <- getLine
  return $ parseCalculatorInput inputStr
  -- end replace

  -- task d)
  -- Referential transparency

  -- task e)
main' :: IO ()
main' = do
  let initValue = 0
      initOp = (\_ y -> y)
      initCounter = 0

  putStrLn "Welcome to the simple Haskell calculator!"
  putStrLn $ show initValue
  calcTimes <- simpleCalculator' initValue initOp initCounter

  if mod calcTimes 2 == 1
  then putStrLn "Good Bye!"
  else putStrLn "Auf wiedersehen!"
  return ()

simpleCalculator' :: Int -> (Int -> Int -> Int) -> Int -> IO Int
simpleCalculator' ans op times = do
  let
    matchInput :: CalculatorInput -> IO Int
    matchInput Exit = return times
    matchInput (Error str) = do
              putStrLn $ "Invalid Input: " ++ str
              putStrLn $ show ans
              simpleCalculator' ans op times
    matchInput (Operator newOp) = simpleCalculator' ans newOp times
    matchInput (Number num) = do
              let newAns = op ans num
              putStrLn $ show newAns
              simpleCalculator' newAns op (times + 1)

  newInput <- getInput
  matchInput newInput
