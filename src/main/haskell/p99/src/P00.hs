module P00 (
    last',
    penultimate,
    kth,
    length',
    reverse',
    isPalindrome,
    flatten, aList, AList(..), -- need "(..)" to export constructors
    compress,
    pack,
    encode, decode, encodeDirect,
    duplicate, duplicateN,
    dropEveryNth,
    split,
    slice,
    rotate,
    removeAt,
    insertAt,
    range,
    randomSelect', randomSelect,
    lotto, lotto',
    randomPermute, randomPermute',
    combinations,
    group3, group,
    lsort, lsortFreq,
    isPrime,
    gcd',
    isCoprimeTo,
    totient,
    primeFactors,
    primeFactorsMultiplicity, primeFactorsMultiplicity',
    totient2,
    runAndMeasure,
    listPrimesInRange,
    goldbachAll, goldbach, goldbachList, goldbachListLimited,
    table2, impl', nand', nor', xor', equ', or', and', not',
    gray,
    huffman
) where

import Data.List(sortBy, find, findIndex)
import Data.Maybe()
import qualified Data.Map.Strict as Map
import System.Random (RandomGen, next, newStdGen)
import Data.Time.Clock.POSIX
import Control.DeepSeq


-- P01
last' :: [a] -> a
last' [] = error "Can't get last element of empty list"
last' [x] = x
last' (_:xs) = last' xs


-- P02
penultimate :: [a] -> a
penultimate [] = error "Can't get penultimate element"
penultimate [_] = error "Can't get penultimate element"
penultimate [x, _] = x
penultimate (_:xs) = penultimate(xs)


-- P03
kth :: Int -> [a] -> a
--kth n xs = xs !! n
kth _ [] = error "Can't get element of empty list"
kth n (x:xs) = if n == 0 then x else kth (n - 1) xs


-- P04
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


-- P05
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


-- P06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


-- P07
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

-- P08
compress :: [Char] -> [Char]
compress [] = []
compress (x:xs) = x : compress (consumeHead x xs)
    where
        consumeHead _ [] = []
        consumeHead char ys =
            if (head ys) /= char then ys
            else consumeHead char (tail ys)

-- P09
pack :: (Eq a) => [a] -> [[a]]
pack' :: (Eq a) => [a] -> [a] -> [[a]]
pack xs = pack' xs []
pack' [] [] = []
pack' [] list = [list]
pack' (x:xs) list =
    if (length list == 0) || x == (head list) then pack' xs (x:list)
    else list : (pack' xs [x])

-- P10
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = (\x -> (length x, head x)) `map` (pack list)

-- P12
decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, c):xs) = (nCopiesOf c n) ++ decode xs

-- P13
encodeDirect :: (Eq a) => [a] -> [(Int, a)]
encodeDirect [] = []
encodeDirect list = (countHeadIn list, head list) : encodeDirect (dropHeadIn list)
    where
        countHeadIn xs = length (takeWhile (== head xs) xs)
        dropHeadIn xs = dropWhile (== head xs) xs

-- P14
duplicate :: (Eq a) => [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x, x] ++ duplicate xs

-- P15
duplicateN :: (Eq a) => Int -> [a] -> [a]
duplicateN _ [] = []
duplicateN n (x:xs) = nCopiesOf x n ++ duplicateN n xs

-- P16
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth _ [] = []
dropEveryNth amount list = drop' amount amount list
    where
        drop' n counter xs
            | n < 2 || null xs = []
            | counter == 1 = drop' n n (tail xs)
            | otherwise = (head xs) : (drop' n (counter - 1) (tail xs))

-- P17
split :: Int -> [a] -> ([a], [a])
split index list = split' index [] list
    where
        split' 0 xs ys = (xs, ys)
        split' _ xs [] = (xs, [])
        split' n xs (y:ys) = split' (n - 1) (xs ++ [y]) ys

-- P18
slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice from to list = fst (split (to - from) (snd (split from list)))

-- P19
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate shift list
    | shift < 0 = rotate (shift + length list) list
    | shift == 0 = []
    | shift > (length list) = rotate (shift `mod` length list) list
    | otherwise = (snd tuple) ++ (fst tuple)
        where tuple = split shift list

-- P20
removeAt :: Int -> [a] -> ([a], a)
removeAt _ [] = error "Cannot remove element from empty list"
removeAt n list =
    let tuple = split n list
        newList = (fst tuple) ++ (tail (snd tuple))
        removeElement = (head (snd tuple))
    in (newList, removeElement)

-- P21
insertAt :: Int -> a -> [a] -> [a]
insertAt _ value [] = [value]
insertAt n value list = (fst s) ++ [value] ++ (snd s)
    where s = split n list

-- P22
range :: Int -> Int -> [Int]
range from to
    | from == to = [to]
    | from < to = from : range (from + 1) to
    | otherwise = []

-- P23
randomSelect :: Int -> [a] -> IO [a]
randomSelect amount list = do
    g <- newStdGen
    return $ randomSelect' g amount list

randomSelect' :: (RandomGen g) => g -> Int -> [a] -> [a]
randomSelect' _ _ [] = []
randomSelect' _ 0 _ = []
randomSelect' randomGen amount list = element : (randomSelect' newGenerator (amount - 1) updatedList)
    where (randomInt, newGenerator) = next randomGen
          index = randomInt `mod` (length list)
          element = kth index list
          updatedList = fst (removeAt index list)

-- P24
lotto :: Int -> Int -> IO [Int]
lotto amount maxNumber = do
    g <- newStdGen
    return $ lotto' g amount maxNumber

lotto' :: (RandomGen g) => g -> Int -> Int -> [Int]
lotto' randomGen amount maxNumber = randomSelect' randomGen amount (range 1 maxNumber)

-- P25
randomPermute :: [a] -> IO [a]
randomPermute list =
    newStdGen >>= (\ g -> return $ randomPermute' g list)

randomPermute' :: (RandomGen g) => g -> [a] -> [a]
randomPermute' _ [] = []
randomPermute' randomGen list = randomSelect' randomGen (length list) list

-- P26
combinations :: Int -> [a] -> [[a]]
combinations amount list
    | amount == 0 = [[]]
    | amount > (length list) = []
    | otherwise =
        (\ subCombination -> head list : subCombination) `map` (combinations (amount - 1) (tail list)) ++
        (combinations amount $ tail list)

-- P27
group3 :: (Eq a) => [a] -> [[[a]]]
group3 list
    | length list /= 9 = error ("Expected group size to be 9 but was " ++ show (length list))
    | otherwise =
        (combinations 2 list) >>= (\comb2 ->
            (combinations 3 $ exclude comb2 list) >>= (\comb3 ->
                (combinations 4 $ exclude comb3 $ exclude comb2 list) >>= (\comb4 -> [[comb2, comb3, comb4]])
            ))
        where
            exclude comb xs = filter (\it -> notElem it comb) xs

-- P28
group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group sizes list
    | sizes == [] = [[]]
    | otherwise =
        (combinations (head sizes) list) >>=
            (\combination -> (\it -> combination : it) `map` (group (tail sizes) (exclude combination list)))
        where
            exclude comb xs = filter (\it -> notElem it comb) xs


lsort :: [[a]] -> [[a]]
lsort listOfLists = sortBy (\a b -> compare (length a) (length b)) listOfLists


lsortFreq :: [[a]] -> [[a]]
lsortFreq listOfLists = sortBy (\a b -> compare (lengthFreqOf a) (lengthFreqOf b)) listOfLists
    where lengthFreqOf list = length (filter (\it -> (length it) == (length list)) listOfLists)


-- P31
isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = notDivisable n (n - 1)
notDivisable :: Int -> Int -> Bool
notDivisable n n2
    | n2 < 2 = True
    | otherwise = ((n `rem` n2) /= 0) && (notDivisable n (n2 - 1))


-- P32
gcd' :: Int -> Int -> Int
gcd' a b
    | a < b = gcd' b a
    | a `rem` b == 0 = b
    | otherwise = gcd b (a - b)


-- P33
isCoprimeTo :: Int -> Int -> Bool
isCoprimeTo a b = (gcd' a b) == 1


-- P34
totient :: Int -> Int
totient n = amountOfComprimes [1..n]
    where amountOfComprimes = length . filter (isCoprimeTo n)

-- P35
primeFactors :: Int -> [Int]
primeFactors n = case firstPrimeOf n of
        Just value -> value : primeFactors (n `div` value)
        Nothing -> []
    where firstPrimeOf number = find (\it -> (isPrime it) && (number `rem` it == 0)) [2..n]


-- P36
primeFactorsMultiplicity :: Int -> [(Int, Int)]
primeFactorsMultiplicity n = case firstPrimeOf n of
        Just value -> add value (primeFactorsMultiplicity (n `div` value))
        Nothing -> []
    where
        firstPrimeOf number = find (\it -> (isPrime it) && (number `rem` it == 0)) [2..n]
        add value list = case findIndex (\it -> (fst it) == value) list of
            Just index -> let entry = (list !! index) in
                (fst entry, (snd entry) + 1) : (filter (\it -> (fst it) /= value) list)
            Nothing -> (value, 1) : list


primeFactorsMultiplicity' :: Int -> Map.Map Int Int
primeFactorsMultiplicity' n = case firstPrimeOf n of
        Just value -> add value (primeFactorsMultiplicity' (n `div` value))
        Nothing -> Map.empty
    where
        firstPrimeOf number = find (\it -> (isPrime it) && (number `rem` it == 0)) [2..n]
        add value result = case Map.lookup value result of
            Just amount -> Map.insert value (amount + 1) result
            Nothing -> Map.insert value 1 $ result


-- http://stackoverflow.com/questions/6400568/exponentiation-in-haskell
-- P37
totient2 :: Int -> Int
totient2 n = foldl (\acc entry -> acc * (phi (fst entry) (snd entry))) 1 factors
    where
        factors = (Map.toList (primeFactorsMultiplicity' n))
        phi p m = (p - 1) * (p ^ (m - 1))

-- P38
-- see test


-- P39
listPrimesInRange :: [Int] -> [Int]
listPrimesInRange valuesRange = filter isPrime valuesRange


goldbachAll :: Int -> [(Int, Int)]
goldbachAll n = filter (sumsUpTo n) primePairs
    where sumsUpTo value it = (fst it) + (snd it) == value
          primePairs = [(i, j) | i <- primes, j <- primes]
          primes = listPrimesInRange [2..n]

goldbach' :: Int -> [Int] -> Maybe (Int, Int)
goldbach' n primes = find (sumsUpTo n) primePairs
     where sumsUpTo value it = (fst it) + (snd it) == value
           primePairs = [(i, j) | i <- primes, j <- primes]

-- P40
goldbach :: Int -> Maybe (Int, Int)
goldbach n = goldbach' n (listPrimesInRange [2..n])


-- P41
goldbachList :: [Int] -> Map.Map Int (Int, Int)
goldbachList aRange = Map.fromList nonEmptyResults
    where
        primes = listPrimesInRange [2..(last aRange)]
        nonEmptyResults = foldl notNothing [] allResults -- :: [Int, (Int, Int)]
        allResults = map (\it -> (it, goldbach' it primes)) aRange -- :: [Int, Maybe (Int, Int)]
        notNothing acc x = case snd x of
            Just value -> (fst x, value) : acc
            Nothing -> acc
goldbachListLimited :: [Int] -> Int -> Map.Map Int (Int, Int)
goldbachListLimited list limit = Map.fromList $ filter (primesAbove limit) (Map.toList $ goldbachList list)
    where primesAbove n entry = (fst $ snd entry) >= n && (snd $ snd entry) >= n


-- P46
not' :: Bool -> Bool
not' False = True
not' True = False

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' a b = not' $ equ' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

table2 :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table2 f = [(a, b, (f a b)) | a <- [True, False], b <- [True, False]]


-- P47,48 skipped


-- P49
gray :: Int -> [String]
gray 0 = [""]
gray n = (\it -> "0" ++ it) `map` prevGrayCode ++
         (\it -> "1" ++ it) `map` reverse prevGrayCode
         where prevGrayCode = gray (n - 1)

-- P50
huffman :: [(Char, Int)] -> [(Char, String)]
huffman code = []
--    where q1 = PQ.empty :: PQ.PQueue Int Int
--          q2 = PQ.empty :: PQ.PQueue Int Int

data Tree a =
    Leaf { value :: a } |
    Node { left :: Tree a, right :: Tree a }
    deriving (Show, Eq)



-- internal functions

nCopiesOf :: a -> Int -> [a]
nCopiesOf _ 0 = []
nCopiesOf value amount = value : nCopiesOf value (amount - 1)

currentMillis :: IO Int
currentMillis = fmap (round . (* 1000)) getPOSIXTime

-- http://programmers.stackexchange.com/questions/160580/how-to-force-evaluation-in-haskell/160587#160587
runAndMeasure :: NFData a => a -> IO (a, Int)
runAndMeasure result = do
        start <- currentMillis
        end <- result `Control.DeepSeq.deepseq` currentMillis
        return (result, end - start)
