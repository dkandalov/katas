import Test.HUnit

last_ :: [a] -> a
last_ [] = error "Can't get last element of empty list"
last_ [x] = x
last_ (_:xs) = last_ xs

penultimate :: [a] -> a
penultimate [] = error "Can't get penultimate element"
penultimate [_] = error "Can't get penultimate element"
penultimate [x, _] = x
penultimate (x:xs) = penultimate(xs)


main :: IO Counts
main =
    do
        runTestTT (TestCase (assertEqual "P01" 8 (last_ [1, 1, 2, 3, 5, 8])))
        runTestTT (TestCase (assertEqual "P02" 5 (penultimate [1, 1, 2, 3, 5, 8])))
