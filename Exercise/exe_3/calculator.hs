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
  return ()
  -- end replace

simpleCalculator :: Int -> (Int -> Int -> Int) -> IO ()
simpleCalculator ans op =  do
  -- task b)
  -- replace with implementation:
  return ()
  -- end replace

getInput :: IO CalculatorInput
getInput = do
  -- task c)
  -- replace with implementation:
  return Exit
  -- end replace

