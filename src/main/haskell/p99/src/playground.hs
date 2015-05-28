main :: IO ()
main = do
    putStrLn "hello"
    putStrLn $ show $ [(i,j) | i <- "ab", j <- [1..4]]
    putStrLn $ show $ [(i,j) | i <- ["ab", "cd", "fgh"], j <- i]
    putStrLn $ show $ [j | i <- ["ab", "cd", "fgh"], j <- i]
    putStrLn $ show $
        do i <- ["ab", "cd", "fgh"]
           j <- i
           return j