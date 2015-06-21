import Test.HUnit
import P00
import System.Random (mkStdGen)
import qualified Data.Map.Strict as Map

main :: IO Counts
main =
    do
        let runSlow = False

        runTestTT $ TestCase $ assertEqual "P01" 8 (last' [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P02" 5 (penultimate [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P03" 2 (kth 2 [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P04" 6 (length' [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P05" [8, 5, 3, 2, 1, 1] (reverse' [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P06" False (isPalindrome [1, 2, 3, 4, 5])
        runTestTT $ TestCase $ assertEqual "P06" True (isPalindrome [1, 2, 3, 2, 1])
        runTestTT $ TestCase $ assertEqual "P07" [1, 1, 2] (flatten [aList([1, 1]), Value 2])
        runTestTT $ TestCase $ assertEqual "P08" "abcade" (compress "aaaabccaadeeee")
        runTestTT $ TestCase $ assertEqual "P09" ["aaaa", "b", "cc", "aa", "d", "eeee"] (pack "aaaabccaadeeee")

        runTestTT $ TestCase $ assertEqual "P10" [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (encode "aaaabccaadeeee")
        runTestTT $ TestCase $ assertEqual "P11" "" "" -- not implemented because it's cumbersome to do in type system
        runTestTT $ TestCase $ assertEqual "P12" "aaaabccaadeeee" (decode [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')])
        runTestTT $ TestCase $ assertEqual "P13" [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (encodeDirect "aaaabccaadeeee")
        runTestTT $ TestCase $ assertEqual "P14" "aabbccccdd" (duplicate "abccd")
        runTestTT $ TestCase $ assertEqual "P15" "aaabbbccccccddd" (duplicateN 3 "abccd")
        runTestTT $ TestCase $ assertEqual "P16" "abdeghjk" (dropEveryNth 3 "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P17" ("abc", "defghijk") (split 3 "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P18" "defg" (slice 3 7 "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P19" "defghijkabc" (rotate 3 "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P19" "defghijkabc" (rotate 14 "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P19" "jkabcdefghi" (rotate (-2) "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P20" ("acd", 'b') (removeAt 1 "abcd")

        runTestTT $ TestCase $ assertEqual "P21" ("a!bcd") (insertAt 1 '!' "abcd")
        runTestTT $ TestCase $ assertEqual "P22" [4, 5, 6, 7, 8, 9] (range 4 9)
        runTestTT $ TestCase $ assertEqual "P22" [] (range 9 4)
        runTestTT $ TestCase $ assertEqual "P23" "hgc" (randomSelect' (mkStdGen 123) 3 "abcdefghijk")
        runTestTT $ TestCase $ assertEqual "P24" [24,23,18,4,13,25] (lotto' (mkStdGen 123) 6 49)
        runTestTT $ TestCase $ assertEqual "P25" "acbdfe" (randomPermute' (mkStdGen 123) "abcdef")
        runTestTT $ TestCase $ assertEqual "P26" [""] (combinations 0 "a")
        runTestTT $ TestCase $ assertEqual "P26" ["a"] (combinations 1 "a")
        runTestTT $ TestCase $ assertEqual "P26" [] (combinations 2 "a")
        runTestTT $ TestCase $ assertEqual "P26" ["ab", "ac", "bc"] (combinations 2 "abc")
        runTestTT $ TestCase $ assertEqual "P26" ["abc"] (combinations 3 "abc")
        runTestTT $ TestCase $ assertEqual "P26"
                ["abc","abd","abe","abf","acd","ace","acf","ade","adf","aef","bcd","bce","bcf","bde","bdf","bef","cde","cdf","cef","def"]
                (combinations 3 "abcdef")
        runTestTT (TestCase (assertEqual "P26" 220 $ (length . combinations 3) "abcdef123456"))
        runTestTT $ TestCase $ assertEqual "P27a" 1260 (length $ group3 ["Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"])
        runTestTT $ TestCase $ assertEqual "P27b" 1260 (length $ group [2, 3, 4] ["Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"])
        runTestTT $ TestCase $ assertEqual "P28a" ["o", "de", "de", "mn", "abc", "fgh", "ijkl"] (lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"])
        runTestTT $ TestCase $ assertEqual "P28b" ["ijkl", "o", "abc", "fgh", "de", "de", "mn"] (lsortFreq ["abc", "de", "fgh", "de", "ijkl", "mn", "o"])

        runTestTT $ TestCase $ assertEqual "P31" False (isPrime 6)
        runTestTT $ TestCase $ assertEqual "P31" True (isPrime 7)
        runTestTT $ TestCase $ assertEqual "P32" 9 (gcd' 36 63)
        runTestTT $ TestCase $ assertEqual "P33" True (35 `isCoprimeTo` 64)
        runTestTT $ TestCase $ assertEqual "P33" False (36 `isCoprimeTo` 64)
        runTestTT $ TestCase $ assertEqual "P34" 4 (totient 10)
        runTestTT $ TestCase $ assertEqual "P35" [] (primeFactors 1)
        runTestTT $ TestCase $ assertEqual "P35" [2, 5] (primeFactors 10)
        runTestTT $ TestCase $ assertEqual "P35" [3, 3, 5, 7] (primeFactors 315)
        runTestTT $ TestCase $ assertEqual "P36" [(3, 2)] (primeFactorsMultiplicity 9)
        runTestTT $ TestCase $ assertEqual "P36" [(3, 2), (5, 1), (7, 1)] (primeFactorsMultiplicity 315)
        runTestTT $ TestCase $ assertEqual "P36" (Map.fromList [(3, 2)]) (primeFactorsMultiplicity' 9)
        runTestTT $ TestCase $ assertEqual "P36" (Map.fromList [(3, 2), (5, 1), (7, 1)]) (primeFactorsMultiplicity' 315)
        runTestTT $ TestCase $ assertEqual "P37" 4 (totient2 10)

        -- P38
        if runSlow then do
            a <- runAndMeasure $ totient 100090
            putStrLn $ "P38 duration: " ++ (show (snd a)) -- ~60ms
            a <- runAndMeasure $ totient2 100090
            putStrLn $ "P38 duration: " ++ (show (snd a)) -- ~3100ms
        else return ()

        runTestTT $ TestCase $ assertEqual "P39" [7, 11, 13, 17, 19, 23, 29, 31] (listPrimesInRange [7..31])
        runTestTT $ TestCase $ assertEqual "P40" [(5,23),(11,17),(17,11),(23,5)] (goldbachAll 28)
        runTestTT $ TestCase $ assertEqual "P40" (Just (5,23)) (goldbach 28)
        runTestTT $ TestCase $ assertEqual "P41"
            (Map.fromList [(9,(2,7)),(10,(3,7)),(12,(5,7)),(13,(2,11)),(14,(3,11)),(15,(2,13)),(16,(3,13)),(18,(5,13)),(19,(2,17)),(20,(3,17))])
            (goldbachList [9..20])

        if runSlow then do
            p41 <- runAndMeasure $ goldbachListLimited [1..2000] 50
            putStrLn $ "P41 duration: " ++ (show (snd p41)) -- ~4.6 seconds
            runTestTT $ TestCase $ assertEqual "P41"
                (Map.fromList [(992,(73,919)),(1382,(61,1321)),(1856,(67,1789)),(1928,(61,1867))])
                (fst p41)
        else return $ Counts 0 0 0 0

        runTestTT $ TestCase $ assertEqual "P46" [
            (True, True, True),
            (True, False, True),
            (False, True, False),
            (False, False, False)
--            ] (table2 (\a b -> (and' a (or' a b))))
            ] (table2 (\a -> and' a . or' a))

        runTestTT $ TestCase $ assertEqual "P49" ["0", "1"] (gray 1)
        runTestTT $ TestCase $ assertEqual "P49" ["00", "01", "11", "10"] (gray 2)
        runTestTT $ TestCase $ assertEqual "P49" ["000", "001", "011", "010", "110", "111", "101", "100"] (gray 3)

        runTestTT $ TestCase $ assertEqual "P50" True (isEmpty emptyQueue)
        runTestTT $ TestCase $ assertEqual "P50" False (isEmpty (Queue [1, 2, 3]))
        runTestTT $ TestCase $ assertEqual "P50" 1 (peek (Queue [1, 2, 3]))
        runTestTT $ TestCase $ assertEqual "P50" (1, Queue [2,3]) (pop (Queue [1, 2, 3]))
        runTestTT $ TestCase $ assertEqual "P50" (Queue [1,2,3,4]) (push 4 (Queue [1, 2, 3]))

        runTestTT $ TestCase $ assertEqual "P50"
            [('a', "1"), ('b', "0")]
            (huffman $ reverse [('a', 45), ('b', 13)])
        runTestTT $ TestCase $ assertEqual "P50"
            [('a', "0"), ('b', "101"), ('c', "100"), ('d', "111"), ('e', "1101"), ('f', "1100")]
            (huffman $ reverse [('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)])
