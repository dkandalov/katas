{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE FlexibleContexts #-}
import Test.HUnit
import Data.List.Ordered(minus, union)
import Data.Array.Unboxed

-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Euler.27s_Sieve
-- http://csharphelper.com/blog/2014/08/use-eulers-sieve-to-find-prime-numbers-in-c/

primesTo :: Int -> [Int]
primesTo m = eratos [2..m]
    where
        eratos [] = []
        eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

primesToQ :: Int -> [Int]
primesToQ m = eratos [2..m]
    where
        eratos []     = []
        eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..m])

primesToG :: Int -> [Int]
primesToG m = 2 : sieve [3,5..m] -- hardcoded to skip multiples of 2
    where
        sieve (p:xs)
           | p*p > m   = p : xs
           | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])


primesToA :: Int -> [Int]
primesToA m = 2 : sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]] :: UArray Int Bool)
    where
        sieve p a
          | p*p > m   = [i | (i,True) <- assocs a]
          | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
          | otherwise = sieve (p+2) a


tests :: [Test]
tests = map (\f ->
    TestCase $ assertEqual "" [2,3,5,7,11,13,17,19] (f 20)
 ) [primesTo, primesToQ, primesToG, primesToA]

main :: IO Counts
main = do runTestTT $ TestList $ map (TestLabel "") tests