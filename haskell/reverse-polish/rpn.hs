-- from http://learnyouahaskell.com/functionally-solving-problems
import Data.List

main = do
    putStrLn $ show $ solveRPN "10 10 10 10 10 sum 4 /"

solveRPN :: String -> Float
solveRPN expression = head $ foldl foldingFunction [] $ words expression
    where foldingFunction (x:y:ys) "*" = (x * y) : ys
          foldingFunction (x:y:ys) "+" = (x + y) : ys
          foldingFunction (x:y:ys) "-" = (y - x) : ys
          foldingFunction (x:y:ys) "/" = (y / x) : ys
          foldingFunction (x:y:ys) "^" = (y ** x) : ys
          foldingFunction (x:xs) "ln" = log x:xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs numberString = read numberString : xs