mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort part1) (mergeSort part2)
    where midIndex = (length xs) `div` 2
          part1 = take midIndex xs
          part2 = drop midIndex xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge xs@(x:restOfX) ys@(y:restOfY)
    | x < y = x : (merge restOfX ys)
    | otherwise = y : (merge xs restOfY)


main = do
    putStrLn $ mergeSort []
    putStrLn $ show $ mergeSort [1]
    putStrLn $ show $ mergeSort [1, 2]
    putStrLn $ show $ mergeSort [2, 1]
    putStrLn $ show $ mergeSort [1, 2, 3]
    putStrLn $ show $ mergeSort [1, 3, 2]
    putStrLn $ show $ mergeSort [2, 1, 3]
    putStrLn $ show $ mergeSort [2, 3, 1]
    putStrLn $ show $ mergeSort [3, 1, 2]
    putStrLn $ show $ mergeSort [3, 2, 1]