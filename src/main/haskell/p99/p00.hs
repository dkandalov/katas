import Test.HUnit
import System.Random (RandomGen, StdGen, next, mkStdGen, newStdGen)

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


-- http://stackoverflow.com/questions/6479444/is-there-a-type-any-in-haskell
data AList a = AList [AList a] | Value a
aList :: [a] -> AList a
aList [] = AList []
aList xs = AList ((\x -> Value x) `map` xs)


flatten :: [AList a] -> [a]
flatten [] = []
flatten (x:xs) = case x of
        Value it -> it : flatten xs
        AList it -> flatten it ++ flatten xs


compress :: [Char] -> [Char]
compress [] = []
compress (x:xs) = x : compress (consumeHead x xs)
    where
        consumeHead _ [] = []
        consumeHead char ys =
            if (head ys) /= char then ys
            else consumeHead char (tail ys)


pack :: (Eq a) => [a] -> [[a]]
pack' :: (Eq a) => [a] -> [a] -> [[a]]
pack xs = pack' xs []
pack' [] [] = []
pack' [] list = [list]
pack' (x:xs) list =
    if (length list == 0) || x == (head list) then pack' xs (x:list)
    else list : (pack' xs [x])


encode :: (Eq a) => [a] -> [(Int, a)]
encode list = (\x -> (length x, head x)) `map` (pack list)


decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, c):xs) = (nCopiesOf c n) ++ decode xs


encodeDirect :: (Eq a) => [a] -> [(Int, a)]
encodeDirect [] = []
encodeDirect list = (countHeadIn list, head list) : encodeDirect (dropHeadIn list)
    where
        countHeadIn xs = length (takeWhile (== head xs) xs)
        dropHeadIn xs = dropWhile (== head xs) xs


duplicate :: (Eq a) => [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x, x] ++ duplicate xs


duplicateN :: (Eq a) => Int -> [a] -> [a]
duplicateN _ [] = []
duplicateN n (x:xs) = nCopiesOf x n ++ duplicateN n xs


dropEveryNth :: Int -> [a] -> [a]
dropEveryNth _ [] = []
dropEveryNth amount list = drop' amount amount list
    where
        drop' n counter xs
            | n < 2 || null xs = []
            | counter == 1 = drop' n n (tail xs)
            | otherwise = (head xs) : (drop' n (counter - 1) (tail xs))


split :: Int -> [a] -> ([a], [a])
split index list = split' index [] list
    where
        split' 0 xs ys = (xs, ys)
        split' _ xs [] = (xs, [])
        split' n xs (y:ys) = split' (n - 1) (xs ++ [y]) ys


slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice from to list = fst (split (to - from) (snd (split from list)))


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate shift list
    | shift < 0 = rotate (shift + length list) list
    | shift == 0 = []
    | shift > (length list) = rotate (shift `mod` length list) list
    | otherwise = (snd tuple) ++ (fst tuple)
        where tuple = split shift list


removeAt :: Int -> [a] -> ([a], a)
removeAt _ [] = error "Cannot remove element from empty list"
removeAt n list =
    let tuple = split n list
        newList = (fst tuple) ++ (tail (snd tuple))
        removeElement = (head (snd tuple))
    in (newList, removeElement)


insertAt :: Int -> a -> [a] -> [a]
insertAt n value [] = [value]
insertAt n value list = (fst s) ++ [value] ++ (snd s)
    where s = split n list


range :: Int -> Int -> [Int]
range from to
    | from == to = [to]
    | from < to = from : range (from + 1) to
    | otherwise = []


randomSelect :: Int -> [a] -> IO [a]
randomSelect amount list = do
    g <- newStdGen
    return $ randomSelect' g amount list

randomSelect' :: (RandomGen g) => g -> Int -> [a] -> [a]
randomSelect' _ _ [] = []
randomSelect' _ 0 _ = []
randomSelect' randomGen amount list = element : (randomSelect' randomGen (amount - 1) updatedList)
    where index = fst (next randomGen) `mod` (length list)
          element = kth index list
          updatedList = fst (removeAt index list)


-- private
nCopiesOf :: a -> Int -> [a]
nCopiesOf _ 0 = []
nCopiesOf value amount = value : nCopiesOf value (amount - 1)



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
        runTestTT (TestCase (assertEqual "P09" ["aaaa", "b", "cc", "aa", "d", "eeee"] (pack "aaaabccaadeeee")))
        runTestTT (TestCase (assertEqual "P10" [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (encode "aaaabccaadeeee")))
        runTestTT (TestCase (assertEqual "P11" "" "")) -- not implemented because it's cumbersome to do in type system
        runTestTT (TestCase (assertEqual "P12" "aaaabccaadeeee" (decode [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')])))
        runTestTT (TestCase (assertEqual "P13" [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (encodeDirect "aaaabccaadeeee")))
        runTestTT (TestCase (assertEqual "P14" "aabbccccdd" (duplicate "abccd")))
        runTestTT (TestCase (assertEqual "P15" "aaabbbccccccddd" (duplicateN 3 "abccd")))
        runTestTT (TestCase (assertEqual "P16" "abdeghjk" (dropEveryNth 3 "abcdefghijk")))
        runTestTT (TestCase (assertEqual "P17" ("abc", "defghijk") (split 3 "abcdefghijk")))
        runTestTT (TestCase (assertEqual "P18" "defg" (slice 3 7 "abcdefghijk")))
        runTestTT (TestCase (assertEqual "P19" "defghijkabc" (rotate 3 "abcdefghijk")))
        runTestTT (TestCase (assertEqual "P19" "defghijkabc" (rotate 14 "abcdefghijk")))
        runTestTT (TestCase (assertEqual "P19" "jkabcdefghi" (rotate (-2) "abcdefghijk")))
        runTestTT (TestCase (assertEqual "P20" ("acd", 'b') (removeAt 1 "abcd")))
        runTestTT (TestCase (assertEqual "P21" ("a!bcd") (insertAt 1 '!' "abcd")))
        runTestTT (TestCase (assertEqual "P22" [4, 5, 6, 7, 8, 9] (range 4 9)))
        runTestTT (TestCase (assertEqual "P22" [] (range 9 4)))
        runTestTT (TestCase (assertEqual "P23" "hei" (randomSelect' (mkStdGen 123) 3 "abcdefghijk")))
