import Test.HUnit

binarySearch value list = doBinarySearch value list 0
doBinarySearch value [] shift = -1
doBinarySearch value list shift =
    let midIndex = (length list) `div` 2
        midValue = list !! midIndex
    in
        if (value == midValue) then midIndex + shift
        else if (value < midValue) then doBinarySearch value (take midIndex list) shift
        else doBinarySearch value (drop (midIndex + 1) list) (shift + midIndex + 1)

tests = test [
    "" ~: assertEqual "" (-1) (binarySearch 1 []),

    "" ~: assertEqual "" (-1) (binarySearch 0 [1]),
    "" ~: assertEqual "" (0)  (binarySearch 1 [1]),
    "" ~: assertEqual "" (-1) (binarySearch 2 [1]),

    "" ~: assertEqual "" (-1) (binarySearch 0 [1, 2]),
    "" ~: assertEqual "" (0)  (binarySearch 1 [1, 2]),
    "" ~: assertEqual "" (1)  (binarySearch 2 [1, 2]),
    "" ~: assertEqual "" (-1) (binarySearch 3 [1, 2]),

    "" ~: assertEqual "" (-1) (binarySearch 0 [1, 2, 3]),
    "" ~: assertEqual "" (0)  (binarySearch 1 [1, 2, 3]),
    "" ~: assertEqual "" (1)  (binarySearch 2 [1, 2, 3]),
    "" ~: assertEqual "" (2)  (binarySearch 3 [1, 2, 3]),
    "" ~: assertEqual "" (-1) (binarySearch 4 [1, 2, 3]),

    "" ~: assertEqual "" (-1) (binarySearch 0 [1, 2, 3, 4]),
    "" ~: assertEqual "" (0)  (binarySearch 1 [1, 2, 3, 4]),
    "" ~: assertEqual "" (1)  (binarySearch 2 [1, 2, 3, 4]),
    "" ~: assertEqual "" (2)  (binarySearch 3 [1, 2, 3, 4]),
    "" ~: assertEqual "" (3)  (binarySearch 4 [1, 2, 3, 4]),
    "" ~: assertEqual "" (-1) (binarySearch 5 [1, 2, 3, 4]),

    "" ~: assertEqual "" (-1) (binarySearch "a" ["b"]),
    "" ~: assertEqual "" (0)  (binarySearch "b" ["b"]),
    "" ~: assertEqual "" (-1) (binarySearch "c" ["b"])]

main = runTestTT tests