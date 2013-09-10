myGCD :: Int -> Int -> Int
myGCD a b =
    if a < b then myGCD b a
    else if a `rem` b == 0 then b
    else myGCD b (a `rem` b)

main = putStrLn $ show $ myGCD 16 12