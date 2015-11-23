import Test.HUnit
import P9x.P00.P00_
import Data.List(nub)
import System.Random(setStdGen, mkStdGen)
import P9x.Util(doExpectEqual)


main :: IO Counts
main = do
    (\f -> doExpectEqual "P01" 8 (f [1, 1, 2, 3, 5, 8])) `mapM_`
        [myLast', myLast'', myLast''', myLast'''', myLast''''']

    (\f -> doExpectEqual "P02" 5 (f [1, 1, 2, 3, 5, 8])) `mapM_`
        [myButLast, myButLast', myButLast'', myButLast''', myButLast'''', lastbut1]

    doExpectEqual "P02" (Just 5) (lastbut1safe [1, 1, 2, 3, 5, 8])

    (\f -> doExpectEqual "P03" 2 (f 2 [1, 1, 2, 3, 5, 8])) `mapM_`
        [elementAt, elementAt', elementAt'', elementAt''', elementAt_w'pf]

    (\f -> doExpectEqual "P04" 6 (f [1, 1, 2, 3, 5, 8])) `mapM_`
        [myLength, myLength1', myLength2', myLength3', myLength4', myLength5', myLength6', myLength1'', myLength2'', myLength3'']

    (\f -> doExpectEqual "P05" [8, 5, 3, 2, 1, 1] (f [1, 1, 2, 3, 5, 8])) `mapM_`
        [reverse', reverse'', reverse''', reverse'''']

    let isPalindromFunctions = [isPalindrome, isPalindrome'1, isPalindrome'2, isPalindrome'3,
                                isPalindrome'4, isPalindrome'5, isPalindrome'6, isPalindrome'7] :: [[Int] -> Bool]
    (\f -> doExpectEqual "P06" False (f [1, 2, 3, 4, 5])) `mapM_` isPalindromFunctions
    (\f -> doExpectEqual "P06" True (f [1, 2, 3, 2, 1])) `mapM_` isPalindromFunctions

    let flattenFunctions = [flatten, flatten', flatten'2, flatten'3, flatten'4, flatten'5, flatten'6]
    (\f -> doExpectEqual "P07" [1, 2] (f $ List[Elem 1, Elem 2])) `mapM_` flattenFunctions
    (\f -> doExpectEqual "P07" [1, 2, 3] (f $ List[nestedList [1, 2], Elem 3])) `mapM_` flattenFunctions
    (\f -> doExpectEqual "P07" [1, 2, 3] (f $ List[List[Elem 1, Elem 2], List[Elem 3]])) `mapM_` flattenFunctions

    let compressFunctions = [compress, compress', compress'2, compress'3, compress'4, compress'5, compress'6, compress'7] :: [[Char] -> [Char]]
    (\f -> doExpectEqual "P08" "abcade" (f "aaaabccaadeeee")) `mapM_` compressFunctions

    let packFunctions = [pack, pack', pack'2, pack'3, pack'4, pack'5]
    (\f -> doExpectEqual "P09" ["aaaa", "b", "cc", "aa", "d", "eeee"] (f "aaaabccaadeeee")) `mapM_` packFunctions

    let encodeFunctions = [encode, encode', encode'2, encode'3, encode'4, encode'5, encode'6, encode'7]
    (\f -> doExpectEqual "P10" [(4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')] (f "aaaabccaadeeee")) `mapM_` encodeFunctions

    let encodeModifiedFunctions = [encodeModified, encodeModified'] :: Eq a => [[a] -> [ListItem a]]
    (\f -> doExpectEqual "P11"
        [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
        (f "aaaabccaadeeee")) `mapM_` encodeModifiedFunctions

    let decodeFunctions = [decode, decode', decode'2] :: Eq a => [[(Int, a)] -> [a]]
    (\f -> doExpectEqual "P12" "aaaabccaadeeee"
        (f [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]))
        `mapM_` decodeFunctions

    let decodeModifiedFunctions = [decodeModified, decodeModified', decodeModified'2, decodeModified'3]
    (\f -> doExpectEqual "P12"
        "aaaabccaadeeee"
        (f [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']))
        `mapM_` decodeModifiedFunctions

    let encodeDirectFunctions = [encodeDirect, encodeDirect']
    (\f -> doExpectEqual "P13"
        [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
        (f "aaaabccaadeeee")) `mapM_` encodeDirectFunctions

    let duplicateFunctions = [dupli, dupli', dupli'2, dupli'3, dupli'4, dupli'5, dupli'6, dupli'7, dupli'8, dupli'9]
    (\f -> doExpectEqual "P14" "aabbccccdd" (f "abccd")) `mapM_` duplicateFunctions

    let replicateFunctions = [repli, repli', repli'2, repli'3]
    (\f -> doExpectEqual "P15" "aaabbbccccccddd" (f "abccd" 3)) `mapM_` replicateFunctions

    let dropEveryFunctions = [dropEvery, dropEvery', dropEvery'2, dropEvery'3, dropEvery'4, dropEvery'5, dropEvery'6, dropEvery'7, dropEvery'8, dropEvery'9, dropEvery'10]
    (\f -> doExpectEqual "P16" "abdeghjk" (f "abcdefghijk" 3)) `mapM_` dropEveryFunctions

    let splitFunctions = [split, split', split'2, split'3, split'4, split'5, split'6, split'7]
    (\f -> doExpectEqual "P17" ("abc", "defghijk") (f "abcdefghijk" 3)) `mapM_` splitFunctions

    let sliceFunctions = [slice, slice'2, slice'3, slice'4, slice'5, slice'6]
    (\f -> doExpectEqual "P18" "cdefg" (f "abcdefghijk" 3 7)) `mapM_` sliceFunctions

    let rotateFunctions = [rotate, rotate', rotate'2, rotate'3, rotate'4, rotate'5, rotate'6, rotate'7]
    (\f -> doExpectEqual "P19" "defghijkabc" (f "abcdefghijk" 3)) `mapM_` rotateFunctions

    let removeAtFunctions = [removeAt, removeAt', removeAt'2, removeAt'3, removeAt'4]
    (\f -> doExpectEqual "P20" ('b', "acd") (f 1 "abcd")) `mapM_` removeAtFunctions

    let insertAtFunctions = [insertAt, insertAt', insertAt'', insertAt''']
    (\f -> doExpectEqual "P21" ("a!bcd") (f '!' "abcd" 1)) `mapM_` insertAtFunctions

    let rangeFunctions = [range, range2, range3, range4, range5, range6]
    (\f -> doExpectEqual "P22" [] (f 4 3)) `mapM_` rangeFunctions
    (\f -> doExpectEqual "P22" [4] (f 4 4)) `mapM_` rangeFunctions
    (\f -> doExpectEqual "P22" [4, 5, 6, 7, 8, 9] (f 4 9)) `mapM_` rangeFunctions


    let randomSelectFunctionsIO = [rnd_select, rnd_select2, rnd_select3, rnd_select4, rnd_select5]
    (\f ->
        do setStdGen $ mkStdGen 234
           result <- f [1,2,3,4,5] 3
           let desc = "P23 IO" ++ (show result)
           doExpectEqual desc 3 (length result)
           doExpectEqual desc True (all (\it -> it>=1 && it<=5) result)
           -- It's not clear from the problem description if selected elements have to be unique.
           -- The above solutions return non-unique results.
           -- doExpectEqual desc True ((length $ nub result) == length result)
     ) `mapM_` randomSelectFunctionsIO

    -- skip diff_select2 because it uses rnd_select which doesn't return unique numbers
    -- skip diff_select3 because it doesn't return unique numbers
    -- skip diff_select4,5 because of different function type and non-unique result
    let diffSelectFunctionsIO = [diff_select]
    (\f ->
        do setStdGen $ mkStdGen 234
           result <- f 3 10
           let desc = "P24 IO " ++ (show result)
           doExpectEqual desc 3 (length result)
           doExpectEqual desc True (all (\it -> it>=1 && it<=10) result)
           doExpectEqual desc True ((length $ nub result) == length result)
     ) `mapM_` diffSelectFunctionsIO

    let rndPermFunctionsIO = [rnd_perm2, rnd_perm3, rnd_perm4]
    (\f ->
        do setStdGen $ mkStdGen 123
           result <- f [1,2,3,4]
           let desc = "P25 IO" ++ (show result)
           doExpectEqual desc 4 (length result)
           doExpectEqual desc True (all (\it -> it>=1 && it<=4) result)
           doExpectEqual desc True ((length $ nub result) == length result)
     ) `mapM_` rndPermFunctionsIO

    let combinationFunctions = [combinations, combinations2, combinations3, combinations4,
                                combinations5, combinations6, combinations7]
    (\f ->
        do doExpectEqual "P26" [[]] (f 0 "")
           doExpectEqual "P26" ["a", "b", "c"] (f 1 "abc")
           doExpectEqual "P26" ["ab", "ac", "bc"] (f 2 "abc")
           doExpectEqual "P26" ["abc"] (f 3 "abc")
     ) `mapM_` combinationFunctions

    let groupFunctions = [groupSubsets, groupSubsets2, groupSubsets3]
    (\f ->
        do doExpectEqual "P27" [[]] (f [] "")
           doExpectEqual "P27" [] (f [1] "")
           doExpectEqual "P27" [["a"]] (f [1] "a")
           doExpectEqual "P27" [["a"], ["b"]] (f [1] "ab")
           doExpectEqual "P27" [["a", "b"], ["b", "a"]] (f [1, 1] "ab")
     ) `mapM_` groupFunctions

    -- TODO P28
    let lengthSortFunctions = [lsort, lsort2, lsort3, lsort4]
    let lengthFrequencySortFunctions = [lfsort, lfsort2, lfsort3, lfsort4, lfsort5]

    return $ (Counts 0 0 0 0)

