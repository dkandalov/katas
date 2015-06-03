module P00_ (
    myLast', myLast'', myLast''', myLast'''', myLast''''',
    myButLast, myButLast', myButLast'', myButLast''', myButLast'''', lastbut1, lastbut1safe,
    elementAt, elementAt', elementAt'', elementAt''', elementAt_w'pf,
    myLength, myLength1', myLength2', myLength3', myLength4', myLength5', myLength6', myLength1'', myLength2'', myLength3'',
    reverse', reverse'', reverse''', reverse'''',
    isPalindrome, isPalindrome'1, isPalindrome'2, isPalindrome'3, isPalindrome'4, isPalindrome'5, isPalindrome'6, isPalindrome'7,
    NestedList(..), nestedList, flatten, flatten', flatten'2, flatten'3
) where

import Data.Foldable(Foldable, foldMap)
import Control.Monad(liftM2)
import Control.Applicative((<*>))
import Control.Arrow((&&&))


-- solutions from https://wiki.haskell.org/99_questions

-- P01
myLast' = foldr1 (const id)
myLast'' = foldr1 (flip const)
myLast''' = head . reverse
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
--lastbut1 :: Foldable f => f a -> a
lastbut1 = fst . foldl (\(_,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "lastbut1: Empty list"
    err2 = error "lastbut1: Singleton"
--lastbut1safe :: Foldable f => f a -> Maybe a
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
