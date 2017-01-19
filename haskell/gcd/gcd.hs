import Control.Monad.Writer

myGCD :: Int -> Int -> Int
myGCD a b =
    if a < b then myGCD b a
    else if a `rem` b == 0 then b
    else myGCD b (a `rem` b)

-- from http://learnyouahaskell.com/for-a-few-monads-more
gcd2 :: Int -> Int -> Writer [String] Int
gcd2 a b
    | b == 0 = do
        tell ["Result: " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd2 b (a `mod` b)

main = do
    putStrLn $ show $ myGCD 16 12
    putStrLn $ show $ fst $ runWriter $ gcd2 16 12
    mapM_ putStrLn $ snd $ runWriter $ gcd2 16 12