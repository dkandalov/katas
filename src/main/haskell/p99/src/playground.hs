import CsvParsec(parseCSV)

main :: IO ()
main = do
    let s = case (parseCSV "hello,aa\n1,2,3\n") of
            Left parseError -> show parseError
            Right x -> show x
    putStrLn s

--    putStrLn "hello"
--    putStrLn $ show $ [(i,j) | i <- "ab", j <- [1..4]]
--    putStrLn $ show $ [(i,j) | i <- ["ab", "cd", "fgh"], j <- i]
--    putStrLn $ show $ [j | i <- ["ab", "cd", "fgh"], j <- i]
--    putStrLn $ show $
--        do i <- ["ab", "cd", "fgh"]
--           j <- i
--           return j