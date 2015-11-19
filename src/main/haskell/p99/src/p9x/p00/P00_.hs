module P9x.P00.P00_ (
    myLast', myLast'', myLast''', myLast'''', myLast''''',
    myButLast, myButLast', myButLast'', myButLast''', myButLast'''', lastbut1, lastbut1safe,
    elementAt, elementAt', elementAt'', elementAt''', elementAt_w'pf,
    myLength, myLength1', myLength2', myLength3', myLength4', myLength5', myLength6', myLength1'', myLength2'', myLength3'',
    reverse', reverse'', reverse''', reverse'''',
    isPalindrome, isPalindrome'1, isPalindrome'2, isPalindrome'3, isPalindrome'4, isPalindrome'5, isPalindrome'6, isPalindrome'7,
    NestedList(..), nestedList, flatten, flatten', flatten'2, flatten'3, flatten'4, flatten'5, flatten'6,
    compress, compress', compress'2, compress'3, compress'4, compress'5, compress'6, compress'7,
    pack, pack', pack'2, pack'3, pack'4, pack'5,
    encode, encode', encode'2, encode'3, encode'4, encode'5, encode'6, encode'7,
    ListItem(..), encodeModified, encodeModified',
    decode, decode', decode'2,
    decodeModified, decodeModified', decodeModified'2, decodeModified'3,
    encodeDirect, encodeDirect',
    dupli, dupli', dupli'2, dupli'3, dupli'4, dupli'5, dupli'6, dupli'7, dupli'8, dupli'9,
    repli, repli', repli'2, repli'3,
    dropEvery, dropEvery', dropEvery'2, dropEvery'3, dropEvery'4, dropEvery'5, dropEvery'6, dropEvery'7, dropEvery'8, dropEvery'9, dropEvery'10,
    split, split', split'2, split'3, split'4, split'5, split'6, split'7,
    slice, slice'2, slice'3, slice'4, slice'5, slice'6,
    rotate, rotate', rotate'2, rotate'3, rotate'4, rotate'5, rotate'6, rotate'7,
    removeAt, removeAt', removeAt'2, removeAt'3, removeAt'4,
    insertAt, insertAt', insertAt'', insertAt''',
    range, range2, range3, range4, range5, range6,
    rnd_select, rnd_select2, rnd_select3, rnd_select4, rnd_select5,
    diff_select, diff_select2, diff_select3, diff_select4, diff_select5,
    rnd_perm2, rnd_perm3, rnd_perm4,
    combinations, combinations2, combinations3, combinations4, combinations5, combinations6, combinations7
) where

import Data.Foldable(Foldable, foldMap)
import Control.Monad(liftM2)
import Control.Applicative((<*>), (<$>), (<**>))
import Control.Arrow((&&&))
import Data.List(group, findIndex, nub, permutations, tails, subsequences, sort)
import GHC.Exts(build)
import System.Random(RandomGen, StdGen, getStdRandom, randomR, randomRIO, randomRs, getStdGen)
import Control.Monad(replicateM)

-- solutions from https://wiki.haskell.org/99_questions

-- P01
myLast' :: [a] -> a
myLast' = foldr1 (const id)
myLast'' :: [a] -> a
myLast'' = foldr1 (flip const)
myLast''' = head . reverse
myLast'''' :: [a] -> a
myLast'''' = foldl1 (curry snd)
myLast''''' [] = error "No end for empty lists!"
myLast''''' x = x !! (length x - 1)


-- P02
myButLast :: [a] -> a
myButLast = last . init
myButLast' x = reverse x !! 1
myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs
myButLast''' (x:(_:[])) = x
myButLast''' (_:xs) = myButLast''' xs
myButLast'''' = head . tail . reverse
lastbut1 :: Foldable f => f a -> a
lastbut1 = fst . foldl (\(_,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "lastbut1: Empty list"
    err2 = error "lastbut1: Singleton"
lastbut1safe :: Foldable f => f a -> Maybe a
lastbut1safe = fst . foldl (\(_,b) x -> (b,Just x)) (Nothing,Nothing)


-- P03
elementAt :: Int -> [a] -> a
elementAt i list = list !! i

elementAt' :: Int -> [a] -> a
elementAt' 0 (x:_)  = x
elementAt' i (_:xs) = elementAt' (i - 1) xs
elementAt' _ _      = error "Index out of bounds"

elementAt'' n xs
        | length xs < n = error "Index out of bounds"
        | otherwise = fst . last $ zip xs [0..n]

elementAt''' n xs = head $ foldr ($) xs $ replicate n tail

elementAt_w'pf = (last .) . take . (+ 1)


-- P04
myLength :: [a] -> Int
myLength list = myLength_acc list 0
    where
        myLength_acc [] n = n
        myLength_acc (_:xs) n = myLength_acc xs (n + 1)

myLength1' = foldl (\n _ -> n + 1) 0    :: [a] -> Int
myLength2' = foldr (\_ n -> n + 1) 0    :: [a] -> Int
myLength3' = foldr (\_ -> (+1)) 0       :: [a] -> Int
myLength4' = foldr ((+) . (const 1)) 0  :: [a] -> Int
myLength5' = foldr (const (+1)) 0       :: [a] -> Int
myLength6' = foldl (const . (+1)) 0     :: [a] -> Int

myLength1'' xs = snd $ last $ zip xs [1..]  -- Just for fun
myLength2'' = snd . last . (flip zip [1..]) :: [a] -> Int -- Because point-free is also fun
myLength3'' = fst . last . zip [1..]        :: [a] -> Int -- same, but easier


-- P05
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [x]

reverse''' :: [a] -> [a]
reverse''' list = reverse_ list []
  where
    reverse_ [] reversed     = reversed
    reverse_ (x:xs) reversed = reverse_ xs (x:reversed)

reverse'''' :: [a] -> [a]
reverse'''' xs = foldr (\x fId empty -> fId (x : empty)) id xs []


-- P06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome'1 :: (Eq a) => [a] -> Bool
isPalindrome'1 []  = True
isPalindrome'1 [_] = True
isPalindrome'1 xs  = (head xs) == (last xs) && (isPalindrome'1 $ init $ tail xs)

isPalindrome'2 :: (Eq a) => [a] -> Bool -- this seems to be just more vebose version of isPalindrome
isPalindrome'2 xs = foldl (\acc (a,b) -> if a == b then acc else False) True input
    where input = zip xs (reverse xs)

isPalindrome'3 :: (Eq a) => [a] -> Bool
isPalindrome'3 = Control.Monad.liftM2 (==) id reverse

isPalindrome'4 :: (Eq a) => [a] -> Bool
isPalindrome'4 = (==) Control.Applicative.<*> reverse

isPalindrome'5 :: (Eq a) => [a] -> Bool
isPalindrome'5 xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (_:xs) [_] = rev == xs
         p rev xs [] = rev == xs

isPalindrome'6 :: (Eq a) => [a] -> Bool
isPalindrome'6 xs = and $ zipWith (==) xs (reverse xs)

isPalindrome'7 :: (Eq a) => [a] -> Bool
isPalindrome'7 xs = (uncurry (==) . (id &&& reverse)) xs

-- P07
data NestedList a = Elem a | List[NestedList a] deriving (Show, Eq)
nestedList :: [a] -> NestedList a
nestedList xs = List $ (\it -> Elem it) `map` xs

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

flatten' :: NestedList a -> [a]
flatten' (Elem a)      = [a]
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs)
flatten' (List [])     = []

flatten'2 :: NestedList a -> [a]
flatten'2 (Elem x) = return x
flatten'2 (List x) = flatten'2 =<< x

flatten'3 :: NestedList a -> [a]
flatten'3 (Elem x) = [x]
flatten'3 (List x) = foldMap flatten'3 x

flatten'4 :: NestedList a -> [a]
flatten'4 a = flt' a []
  where flt' (Elem x)      xs = x:xs
        flt' (List (x:ls)) xs = flt' x (flt' (List ls) xs)
        flt' (List [])     xs = xs

flatten'5 :: NestedList a -> [a] -- same as version with concatMap above
flatten'5 (Elem x ) = [x]
flatten'5 (List xs) =  foldr (++) [] $ map flatten'5 xs

flatten'6 :: NestedList a -> [a]
flatten'6 = reverse . rec []
  where rec acc (List []) = acc
        rec acc (Elem x)  = x:acc
        rec acc (List (x:xs)) = rec (rec acc x) (List xs)


-- P08
compress :: Eq a => [a] -> [a]
compress = map head . group

compress' :: Eq a => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y    = compress' ys
    | otherwise = x : compress' ys
compress' ys = ys
compress'2 xs = (foldr f (const []) xs) Nothing
  where
    f x r a@(Just q) | x == q = r a
    f x r _ = x : r (Just x)
compress'3 :: Eq a => [a] -> [a]
compress'3 = foldr skipDups []
    where skipDups x [] = [x]
          skipDups x acc
                | x == head acc = acc
                | otherwise = x : acc
compress'4 []     = []
compress'4 (x:xs) = x : (compress'4 $ dropWhile (== x) xs)
compress'5 xs = foldr (\a b -> if a == (head b) then b else a:b) [last xs] xs
compress'6 x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x
compress'7 x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) [head x] x


-- P09
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:first) : pack' rest
         where
           getReps [] = ([], [])
           getReps (y:ys)
                   | y == x = let (f,r) = getReps ys in (y:f, r)
                   | otherwise = ([], (y:ys))
           (first, rest) = getReps xs

pack'2 :: Eq a => [a] -> [[a]]
pack'2 [] = []
pack'2 (x:xs) = (x:reps) : (pack'2 rest)
    where
        (reps, rest) = maybe (xs,[]) (\i -> splitAt i xs)
                             (findIndex (/=x) xs)

pack'3 :: (Eq a) => [a] -> [[a]]
pack'3 [] = []
pack'3 (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

pack'4 :: (Eq a) => [a] -> [[a]]
pack'4 [] = []
pack'4 [x] = [[x]]
pack'4 (x:xs) = if x `elem` (head (pack'4 xs))
                then (x:(head (pack'4 xs))):(tail (pack'4 xs))
                else [x]:(pack'4 xs)

pack'5 :: (Eq a) => [a] -> [[a]]
pack'5 [] = []
pack'5 (y:ys) = reverse $ impl ys [[y]]
    where
        impl [] packed = packed
        impl (x:xs) p@(z:zs)
            | x == (head z) = impl xs ((x:z):zs)
            | otherwise     = impl xs ([x]:p)

-- P10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group

encode'2 :: Eq a => [a] -> [(Int, a)]
encode'2 xs = map (length &&& head) $ group xs

encode'3 :: Eq a => [a] -> [(Int, a)]
encode'3 = map ((,) <$> length <*> head) . pack

encode'4 :: Eq a => [a] -> [(Int, a)]
encode'4 xs = (enc . pack) xs
    where enc = foldr (\x acc -> (length x, head x) : acc) []

encode'5 :: Eq a => [a] -> [(Int, a)]
encode'5 [] = []
encode'5 (x:xs) = (length $ x : takeWhile (==x) xs, x)
                    : encode'5 (dropWhile (==x) xs)

encode'6 :: Eq a => [a] -> [(Int, a)]
encode'6 []     = []
encode'6 (x:xs) = encode'' 1 x xs
        where encode'' n x [] = [(n, x)]
              encode'' n x (y:ys)
                | x == y    = encode'' (n + 1) x ys
                | otherwise = (n, x) : encode'' 1 y ys

encode'7 :: Eq a => [a] -> [(Int, a)]
encode'7 xs = zip (map length l) h
        where l = group xs
              h = map head l

-- P11
data ListItem a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

encodeModified' :: Eq a => [a] -> [ListItem a]
encodeModified' xs = [y | x <- group xs,
    let y = if (length x) == 1 then
                Single (head x) else
                Multiple (length x) (head x)]

-- P12
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

decode :: Eq a => [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)

toTuple :: ListItem a -> (Int, a)
toTuple (Single x)     = (1, x)
toTuple (Multiple n x) = (n, x)

decodeModified' :: [ListItem a] -> [a]
decodeModified' = concatMap (uncurry replicate . toTuple)

decodeModified'2 :: [ListItem a]-> [a]
decodeModified'2 = foldl (\x y -> x ++ decodeHelper y) []
    where decodeHelper :: ListItem a -> [a]
          decodeHelper (Single x)     = [x]
          decodeHelper (Multiple n x) = replicate n x

decodeModified'3 :: [ListItem a] -> [a]
decodeModified'3 = (\acc e ->
        case e of
            Single x -> acc ++ [x]
            Multiple n x -> acc ++ replicate n x
    ) `foldl` []

decode' :: Eq a => [(Int,a)] -> [a]
decode' xs = foldr f [] xs
    where f (1, x) r = x : r
          f (k, x) r = x : f (k-1, x) r

{-# INLINE decode #-}
decode'2 :: Eq a => [(Int,a)] -> [a]
decode'2 xs = build (\c n ->
  let
    f (1, x) r = x `c` r
    f (k, x) r = x `c` f (k-1, x) r
  in
    foldr f n xs)


-- P13
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect_ 1 x xs
encodeDirect_ n y [] = [encodeElement n y]
encodeDirect_ n y (x:xs) | y == x    = encodeDirect_ (n+1) y xs
                         | otherwise = encodeElement n y : (encodeDirect_ 1 x xs)
encodeElement 1 y = Single y
encodeElement n y = Multiple n y

encodeDirect' :: (Eq a)=> [a] -> [ListItem a]
encodeDirect' [] = []
encodeDirect' (x:xs)
    | count == 1  = (Single x) : (encodeDirect' xs)
    | otherwise   = (Multiple count x) : (encodeDirect' rest)
    where
        (matched, rest) = span (==x) xs
        count = 1 + (length matched)

-- P14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs
dupli' list = concat [[x,x] | x <- list]
dupli'2 xs = xs >>= (\x -> [x,x])
dupli'3 = (<**> [id,id])
dupli'4 :: [a] -> [a]
dupli'4 = concatMap (\x -> [x,x])
dupli'5 :: [a] -> [a]
dupli'5 = concatMap (replicate 2)
dupli'6 :: [a] -> [a]
dupli'6 = foldl (\acc x -> acc ++ [x,x]) []
dupli'7 :: [a] -> [a]
dupli'7 = foldr (\ x xs -> x : x : xs) []
dupli'8 :: [a] -> [a]
dupli'8 = foldr (\x -> (x:) . (x:)) []
dupli'9 :: [a] -> [a]
dupli'9 = foldr ((.) <$> (:) <*> (:)) []

-- P15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

repli'2 :: [a] -> Int -> [a]
repli'2 xs n = concatMap (take n . repeat) xs

repli'3 :: [a] -> Int -> [a]
repli'3 xs n = xs >>= replicate n

-- P16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) n = dropEvery' (x:xs) n 1
    where dropEvery' (x:xs) n i =
            (if (n `divides` i) then [] else [x]) ++ (dropEvery' xs n (i+1))
          dropEvery' [] _ _ = []
          divides x y = y `mod` x == 0

dropEvery' :: [a] -> Int -> [a]
dropEvery' list count = helper list count count
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))

dropEvery'2 :: [a] -> Int -> [a]
dropEvery'2 xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)

dropEvery'3 :: [a] -> Int -> [a]
dropEvery'3 [] _ = []
dropEvery'3 list count = (take (count-1) list) ++ dropEvery'3 (drop count list) count

dropEvery'4 :: [a] -> Int -> [a]
dropEvery'4 xs n
      | length xs < n = xs
      | otherwise     = take (n-1) xs ++ dropEvery'4 (drop n xs) n

dropEvery'5 :: [a] -> Int -> [a]
dropEvery'5 = flip $ \n -> map snd . filter ((n/=) . fst) . zip (cycle [1..n])

dropEvery'6 :: [a] -> Int -> [a]
dropEvery'6 xs n = [i | (i,c) <- (zip xs [1,2..]), (mod c n) /= 0]

dropEvery'7 :: [a] -> Int -> [a]
dropEvery'7 xs n = map fst $ filter (\(_,i) -> i `mod` n /= 0) $ zip xs [1..]

dropEvery'8 :: [a] -> Int -> [a]
dropEvery'8 xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

dropEvery'9 :: [a] -> Int -> [a]
dropEvery'9 xs n = snd $
    foldl (\acc e -> if fst acc > 1
        then (fst acc - 1, snd acc ++ [e])
        else (n, snd acc))
    (n, []) xs

dropEvery'10 :: [a] -> Int -> [a]
dropEvery'10 xs n = fst $ foldr
        (\x (xs, i) -> (if mod i n == 0 then xs else x:xs, i - 1))
        ([], length xs) xs

-- P17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)
split' = flip splitAt

split'2 :: [a] -> Int -> ([a], [a])
split'2 [] _ = ([], [])
split'2 list@(x : xs) n
    | n > 0     = (x : ys, zs)
    | otherwise = ([], list)
    where (ys, zs) = split'2 xs (n - 1)

split'3 :: [a] -> Int -> ([a], [a])
split'3 (x:xs) n | n > 0 = (:) x . fst &&& snd $ split'3 xs (n - 1)
split'3 xs _             = ([], xs)

split'4 :: [a] -> Int -> ([a], [a])
split'4 [] _ = ([], [])
split'4 list n
  | n < 0 = (list, [])
  | otherwise  = (first output, second output)
    where output = foldl (\acc e -> if third acc > 0
            then (first acc ++ [e], second acc, third acc - 1)
            else (first acc, second acc ++ [e], third acc)) ([], [], n) list
          first (x, _, _) = x
          second (_, y, _) = y
          third (_, _, z) = z

split'5 :: [a] -> Int -> ([a],[a])
split'5 lst n = snd $ foldl helper (0,([],[])) lst
    where helper (i, (left, right)) x = if i >= n
            then (i + 1, (left, right ++ [x]))
            else (i + 1, (left ++ [x], right))

split'6 :: [a] -> Int -> ([a], [a])
split'6 xs n = let (a, b) = helper [] xs n in (reverse a, b)
  where helper left right@(r:rs) n
         | n == 0    = (left, right)
         | otherwise = helper (r:left) rs (n - 1)

split'7 :: [a] -> Int -> ([a], [a])
split'7 [] _ = ([], [])
split'7 (x:xs) n
  | n > 0  = (x : (fst (split xs (n-1))), snd (split xs (n-1)))
  | n <= 0 = (fst (split xs 0), x : (snd (split xs 0)))

-- P18
slice :: [a] -> Int -> Int -> [a]
slice xs i k | i > 0 = take (k - i + 1) $ drop (i - 1) xs

slice'2 :: [a] -> Int -> Int -> [a]
slice'2 lst 1 m = slice_ lst m []
        where slice_ _ 0 acc = reverse acc
              slice_ (x:xs) n acc = slice_ xs (n - 1) (x:acc)
              slice_ [] _ _ = []
slice'2 (_:xs) n m = slice xs (n - 1) (m - 1)
slice'2 []     _ _ = []

slice'3 :: [a] -> Int -> Int -> [a]
slice'3 [] _ _  = []
slice'3 (x:xs) i k
 | i > 1      = slice'3 xs (i - 1) (k - 1)
 | k < 1      = []
 | otherwise  = x:slice'3 xs (i - 1) (k - 1)

slice'4 :: [a] -> Int -> Int -> [a]
slice'4 xs i j = map snd
        $ filter (\(x,_) -> x >= i && x <= j)
        $ zip [1..] xs

slice'5 :: [a] -> Int -> Int -> [a]
slice'5 xs i k = [x | (x,j) <- zip xs [1..k], i <= j]

slice'6 :: [a] -> Int -> Int -> [a]
slice'6 xs a b = fst $ unzip $ filter ((>=a) . snd) $ zip xs [1..b]

-- P19
rotate :: [a] -> Int -> [a]
rotate xs n = take len . drop (n `mod` len) . cycle $ xs
              where len = length xs

rotate' :: [a] -> Int -> [a]
rotate' xs n = take (length xs) $ drop (length xs + n) $ cycle xs

rotate'2 :: [a] -> Int -> [a]
rotate'2 xs n = if n >= 0 then
                  drop n xs ++ take n xs
                else let l = ((length xs) + n) in
                  drop l xs ++ take l xs

rotate'3 :: [a] -> Int -> [a]
rotate'3 xs n | n >= 0 = drop n xs ++ take n xs
              | n < 0 = drop len xs ++ take len xs
                        where len = n+length xs
rotate'4 :: [a] -> Int -> [a]
rotate'4 xs n = drop nn xs ++ take nn xs
        where
          nn = n `mod` length xs

rotate'5 :: [a] -> Int -> [a]
rotate'5 xs n
    | n < 0 = rotate xs (n+len)
    | n > len = rotate xs (n-len)
    | otherwise = let (f,s) = splitAt n xs in s ++ f
    where len = length xs

-- original version rotates right for positive number what is incosistent with other functions
rotate'6 :: [a] -> Int -> [a]
rotate'6 xs n
    | n > 0 = (drop n xs) ++ (take n xs)
    | n <= 0 = (reverse . take (negate n) . reverse $ xs) ++ (reverse . drop (negate n) . reverse $ xs)

rotate'7 :: [a] -> Int -> [a]
rotate'7 [] _ = []
rotate'7 x 0 = x
rotate'7 x y
  | y > 0 = rotate'7 (tail x ++ [head x]) (y-1)
  | otherwise = rotate'7 (last x : init x) (y+1)

-- P20 (modified to be zero-indexed)
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = case back of
        [] -> error "removeAt: index too large"
        x:rest -> (x, front ++ rest)
  where (front, back) = splitAt k xs

removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs = (xs !! n, take n xs ++ drop (n + 1) xs)

removeAt'2 :: Int -> [a] -> (a, [a])
removeAt'2 n xs = let (front, back) = splitAt n xs in
                  (head back, front ++ tail back)

removeAt'3 :: Int -> [a] -> (a, [a])
removeAt'3 n = (\(a, b) -> (head b, a ++ tail b)) . splitAt n

removeAt'4 :: Int -> [a] -> (a, [a])
removeAt'4 0 (x:xs) = (x, xs)
removeAt'4 n (x:xs) = (l, x:r)
    where (l, r) = removeAt (n - 1) xs

-- P21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys     0 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)

insertAt' x xs n = take n xs ++ [x] ++ drop n xs

insertAt'' el lst n = fst $ foldl helper ([],0) lst
    where helper (acc,i) x = if i == n then (acc++[el,x],i+1) else (acc++[x],i+1)

insertAt''' :: a -> [a] -> Int -> [a]
insertAt''' elt lst pos = foldr concat' [] $ zip [0..] lst
    where concat' (i, x) xs
            | i == pos  = elt:x:xs
            | otherwise = x:xs

-- P22
range :: Int -> Int -> [Int]
range x y = [x..y]

range2 :: Int -> Int -> [Int]
range2 = enumFromTo

range3 x y = take (y-x+1) $ iterate (+1) x

range4 start stop
    | start > stop  = []
    | start == stop = [stop]
    | start < stop  = start:range4 (start+1) stop

-- modified original solution so that it doesn't do reverse ranges
range5 :: (Ord a, Enum a) => a -> a -> [a]
range5 a b | (a > b) = []
range5 a b = a:range5 ((if a <= b then succ else const b) a) b

range6 l r = if (l > r) then [] else scanl (+) l (replicate (r - l) 1)


-- P23
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select l n
    | n < 0 = error "N must be greater than zero."
    | otherwise = do pos <- replicateM n $ getStdRandom $ randomR (0, (length l)-1)
                     return [l!!p | p <- pos]

rnd_select2 :: [a] -> Int -> IO [a]
rnd_select2 xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

rnd_select3 :: [a] -> Int -> IO [a]
rnd_select3 xs n
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)

-- An O(N) algorithm
rnd_select4 :: [a] -> Int -> IO [a]
rnd_select4 _ 0 = return []
rnd_select4 (x:xs) n =
    do r <- randomRIO (0, (length xs))
       if r < n
           then do
               rest <- rnd_select4 xs (n-1)
               return (x : rest)
           else rnd_select4 xs n

-- A solution returns random results even when the number of items we want is the same as the number of items in the list
rnd_select5 :: [a] -> Int -> IO [a]
rnd_select5 _  0 = return []
rnd_select5 [] _ = return []
rnd_select5 xs count =
    do r <- randomRIO (0, (length xs)-1)
       rest <- rnd_select5 (snd $ removeAt r xs) (count-1)
       return ((xs!!r) : rest)


-- P24
diff_select :: Int -> Int -> IO [Int]
diff_select n to = diff_select' n [1..to]

diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0, (length xs)-1)
                       let remaining = take r xs ++ drop (r+1) xs
                       rest <- diff_select' (n-1) remaining
                       return ((xs!!r) : rest)

diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n to = rnd_select [1..to] n

diff_select3 :: Int -> Int -> IO [Int]
diff_select3 n m = do
  gen <- getStdGen
  return . take n $ randomRs (1, m) gen

diff_select4 :: Int -> Int -> StdGen -> [Int]
diff_select4 n m = take n . nub . randomRs (1, m)

diff_select5 :: Int -> Int -> IO [Int]
diff_select5 n m = take n . nub . randomRs (1, m) <$> getStdGen


-- P25
-- The solution below assumes that rnd_select returns unique elements which is not true
--rnd_perm :: [a] -> IO [a]
--rnd_perm xs = rnd_select xs (length xs)

rnd_perm2 :: [a] -> IO [a]
rnd_perm2 []     = return []
rnd_perm2 (x:xs) = do
    index <- randomRIO (0, (length xs))
    rest <- rnd_perm2 xs
    return $ let (ys,zs) = splitAt index rest
             in ys ++ (x:zs)

rnd_perm3 :: [a] -> IO [a]
rnd_perm3 [] = return []
rnd_perm3 xs = do
    index <- randomRIO (0, (length xs)-1)
    rest <- let (ys,(_:zs)) = splitAt index xs
            in rnd_perm3 $ ys ++ zs
    return $ (xs !! index):rest

rnd_perm4 :: [a] -> IO [a]
rnd_perm4 xs = rndElem . permutations $ xs
    where rndElem :: [a] -> IO a
          rndElem xs = do
            index <- randomRIO (0, length xs - 1)
            return $ xs !! index


-- P26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

combinations2 :: Int -> [a] -> [[a]]
combinations2 0 _  = return []
combinations2 n xs = do y:xs' <- tails xs
                        ys <- combinations2 (n-1) xs'
                        return (y:ys)

combinations3 :: Int -> [a] -> [[a]]
combinations3 0 _ = [[]]
combinations3 n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                   , x <- combinations3 (n-1) (drop (i+1) xs) ]

combinations4 :: Int -> [a] -> [[a]]
combinations4 _ [] = [[]]
combinations4 0 _  = [[]]
combinations4 k (x:xs) = x_start ++ others
    where x_start = [ x : rest | rest <- combinations4 (k-1) xs ]
          others  = if k <= length xs then combinations4 k xs else []

combinations5 :: Int -> [a] -> [[a]]
combinations5 0 _ = [[]]
combinations5 _ [] = []
combinations5 n (x:xs) = (map (x:) (combinations5 (n-1) xs)) ++ (combinations5 n xs)

combinations6 :: Int -> [a] -> [[a]]
combinations6 k ns = filter ((k==).length) (subsequences ns)

-- let's try it with combinations 2 [1,2,3]
combinations7 :: (Ord a) => Int -> [a] -> [[a]]
combinations7 n xs = compressed
    where
          -- create all combinations (multiple instances, permutations allowed)
          -- answer : [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
          combinations' n _ | n <= 0 = [[]]
          combinations' 1 xs = map (:[]) xs
          combinations' n xs = (:) <$> xs <*> combinations (n-1) xs
          -- sort every sublist and the list itself after that
          -- [[1,1],[1,2],[1,2],[1,3],[1,3],[2,2],[2,3],[2,3],[3,3]]
          sorted = sort . map sort $ combinations' n xs
          -- for each sublist, eliminate duplicates (see Problem 8)
          -- [[1],[1,2],[1,2],[1,3],[1,3],[2],[2,3],[2,3],[3]]
          grouped = map (map head . group) sorted
          -- filter all sublist with length < n,
          -- this means that they had at least two times the same value in it
          -- [[1,2],[1,2],[1,3],[1,3],[2,3],[2,3]]
          filtered = filter (\xs -> length xs == n) grouped
          -- eliminate duplicates a second time, this time in the list itself
          -- [[1,2],[1,3],[2,3]]
          compressed = map head . group $ filtered