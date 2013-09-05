import Test.HUnit

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge sortedPart1 sortedPart2
    where
        middleIndex = (length xs) `div` 2
        sortedPart1 = mergeSort $ take middleIndex xs
        sortedPart2 = mergeSort $ drop middleIndex xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
    if x < y then x : merge xs (y:ys)
    else y : merge (x:xs) ys


mergeSortShouldSortList = test [
    "" ~: assertEqual "" [] (mergeSort [] :: [Int]),

    "" ~: assertEqual "" [1] (mergeSort [1]),

    "" ~: assertEqual "" [1, 2] (mergeSort [1, 2]),
    "" ~: assertEqual "" [1, 2] (mergeSort [2, 1]),

    "" ~: assertEqual "" [1, 2, 3] (mergeSort [1, 2, 3]),
    "" ~: assertEqual "" [1, 2, 3] (mergeSort [1, 3, 2]),
    "" ~: assertEqual "" [1, 2, 3] (mergeSort [2, 1, 3]),
    "" ~: assertEqual "" [1, 2, 3] (mergeSort [2, 3, 1]),
    "" ~: assertEqual "" [1, 2, 3] (mergeSort [3, 1, 2]),
    "" ~: assertEqual "" [1, 2, 3] (mergeSort [3, 2, 1])
    ]

main = runTestTT mergeSortShouldSortList