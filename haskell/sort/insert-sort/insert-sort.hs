insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort xs = doSort xs []
    where
        doSort [] sorted = sorted
        doSort (x:xs) sorted = doSort xs (insertInto sorted x)
        insertInto [] x = [x]
        insertInto sorted@(firstSorted:restOfSorted) x =
            if x < firstSorted then x : sorted
            else firstSorted : (insertInto restOfSorted x)

main = do
    putStrLn $ show $ (insertSort [] :: [Int])
    putStrLn $ show $ insertSort [1]
    putStrLn $ show $ insertSort [1, 2]
    putStrLn $ show $ insertSort [2, 1]
    putStrLn $ show $ insertSort [1, 2, 3]
    putStrLn $ show $ insertSort [1, 3, 2]
    putStrLn $ show $ insertSort [2, 1, 3]
    putStrLn $ show $ insertSort [2, 3, 1]
    putStrLn $ show $ insertSort [3, 1, 2]
    putStrLn $ show $ insertSort [3, 2, 1]