import Test.HUnit

last' :: [a] -> a
last' [] = error "Can't get last element of empty list"
last' [x] = x
last' (_:xs) = last' xs

penultimate :: [a] -> a
penultimate [] = error "Can't get penultimate element"
penultimate [_] = error "Can't get penultimate element"
penultimate [x, _] = x
penultimate (_:xs) = penultimate(xs)

kth :: Int -> [a] -> a
--kth n xs = xs !! n
kth _ [] = error "Can't get element of empty list"
kth n (x:xs) = if n == 0 then x else kth (n - 1) xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

data AList a = AList [AList a] | Value a
aList :: [a] -> AList a
aList [] = AList []
aList xs = AList ((\x -> Value x) `map` xs)

flatten :: [AList a] -> [a]
flatten [] = []
flatten (head:xs) = case head of
        Value it -> [it] ++ flatten xs
        AList it -> flatten it ++ flatten xs

compress :: [Char] -> [Char]
compress [] = []
compress xs = []

main :: IO Counts
main =
    do
        runTestTT (TestCase (assertEqual "P01" 8 (last' [1, 1, 2, 3, 5, 8])))
        runTestTT (TestCase (assertEqual "P02" 5 (penultimate [1, 1, 2, 3, 5, 8])))
        runTestTT (TestCase (assertEqual "P03" 2 (kth 2 [1, 1, 2, 3, 5, 8])))
        runTestTT (TestCase (assertEqual "P04" 6 (length' [1, 1, 2, 3, 5, 8])))
        runTestTT (TestCase (assertEqual "P05" [8, 5, 3, 2, 1, 1] (reverse' [1, 1, 2, 3, 5, 8])))
        runTestTT (TestCase (assertEqual "P06" False (isPalindrome [1, 2, 3, 4, 5])))
        runTestTT (TestCase (assertEqual "P06" True (isPalindrome [1, 2, 3, 2, 1])))
        runTestTT (TestCase (assertEqual "P07" [1, 1, 2] (flatten [aList([1, 1]), Value 2])))
        runTestTT (TestCase (assertEqual "P08" "abcade" (compress "aaaabccaadeeee")))
