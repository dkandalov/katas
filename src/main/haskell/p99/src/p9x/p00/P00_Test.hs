import Test.HUnit
import P9x.P00.P00_

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Counts
expectEqual desc expected actual = (runTestTT (TestCase (assertEqual desc expected actual)))

main :: IO Counts
main = do
        (\f -> expectEqual "P01" 8 (f [1, 1, 2, 3, 5, 8])) `mapM_`
            [myLast', myLast'', myLast''', myLast'''', myLast''''']

        (\f -> expectEqual "P02" 5 (f [1, 1, 2, 3, 5, 8])) `mapM_`
            [myButLast, myButLast', myButLast'', myButLast''', myButLast'''', lastbut1]

        expectEqual "P02" (Just 5) (lastbut1safe [1, 1, 2, 3, 5, 8])

        (\f -> expectEqual "P03" 2 (f 2 [1, 1, 2, 3, 5, 8])) `mapM_`
            [elementAt, elementAt', elementAt'', elementAt''', elementAt_w'pf]

        (\f -> expectEqual "P04" 6 (f [1, 1, 2, 3, 5, 8])) `mapM_`
            [myLength, myLength1', myLength2', myLength3', myLength4', myLength5', myLength6', myLength1'', myLength2'', myLength3'']

        (\f -> expectEqual "P05" [8, 5, 3, 2, 1, 1] (f [1, 1, 2, 3, 5, 8])) `mapM_`
            [reverse', reverse'', reverse''', reverse'''']

        let isPalindromFunctions = [isPalindrome, isPalindrome'1, isPalindrome'2, isPalindrome'3,
                                    isPalindrome'4, isPalindrome'5, isPalindrome'6, isPalindrome'7] :: [[Int] -> Bool]
        (\f -> expectEqual "P06" False (f [1, 2, 3, 4, 5])) `mapM_` isPalindromFunctions
        (\f -> expectEqual "P06" True (f [1, 2, 3, 2, 1])) `mapM_` isPalindromFunctions

        let flattenFunctions = [flatten, flatten', flatten'2, flatten'3, flatten'4, flatten'5, flatten'6]
        (\f -> expectEqual "P07" [1, 2] (f $ List[Elem 1, Elem 2])) `mapM_` flattenFunctions
        (\f -> expectEqual "P07" [1, 2, 3] (f $ List[nestedList [1, 2], Elem 3])) `mapM_` flattenFunctions
        (\f -> expectEqual "P07" [1, 2, 3] (f $ List[List[Elem 1, Elem 2], List[Elem 3]])) `mapM_` flattenFunctions

        let compressFunctions = [compress, compress', compress'2, compress'3, compress'4, compress'5, compress'6, compress'7] :: [[Char] -> [Char]]
        (\f -> expectEqual "P08" "abcade" (f "aaaabccaadeeee")) `mapM_` compressFunctions

        let packFunctions = [pack, pack', pack'2, pack'3, pack'4, pack'5]
        (\f -> expectEqual "P09" ["aaaa", "b", "cc", "aa", "d", "eeee"] (f "aaaabccaadeeee")) `mapM_` packFunctions

        let encodeFunctions = [encode, encode', encode'2, encode'3, encode'4, encode'5, encode'6, encode'7]
        (\f -> expectEqual "P10" [(4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')] (f "aaaabccaadeeee")) `mapM_` encodeFunctions

        let encodeModifiedFunctions = [encodeModified, encodeModified'] :: Eq a => [[a] -> [ListItem a]]
        (\f -> expectEqual "P11"
            [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
            (f "aaaabccaadeeee")) `mapM_` encodeModifiedFunctions

        let decodeFunctions = [decode, decode', decode'2] :: Eq a => [[(Int, a)] -> [a]]
        (\f -> expectEqual "P12" "aaaabccaadeeee"
            (f [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]))
            `mapM_` decodeFunctions

        let decodeModifiedFunctions = [decodeModified, decodeModified', decodeModified'2, decodeModified'3]
        (\f -> expectEqual "P12"
            "aaaabccaadeeee"
            (f [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']))
            `mapM_` decodeModifiedFunctions

        let encodeDirectFunctions = [encodeDirect, encodeDirect']
        (\f -> expectEqual "P13"
            [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
            (f "aaaabccaadeeee")) `mapM_` encodeDirectFunctions

        let duplicateFunctions = [dupli, dupli', dupli'2, dupli'3, dupli'4, dupli'5, dupli'6, dupli'7, dupli'8, dupli'9]
        (\f -> expectEqual "P14" "aabbccccdd" (f "abccd")) `mapM_` duplicateFunctions

        let replicateFunctions = [repli, repli', repli'2, repli'3]
        (\f -> expectEqual "P15" "aaabbbccccccddd" (f "abccd" 3)) `mapM_` replicateFunctions

        let dropEveryFunctions = [dropEvery, dropEvery', dropEvery'2, dropEvery'3, dropEvery'4, dropEvery'5, dropEvery'6, dropEvery'7, dropEvery'8, dropEvery'9, dropEvery'10]
        (\f -> expectEqual "P16" "abdeghjk" (f "abcdefghijk" 3)) `mapM_` dropEveryFunctions

        let splitFunctions = [split, split', split'2, split'3, split'4, split'5, split'6, split'7]
        (\f -> expectEqual "P17" ("abc", "defghijk") (f "abcdefghijk" 3)) `mapM_` splitFunctions

        let sliceFunctions = [slice, slice'2, slice'3, slice'4, slice'5, slice'6]
        (\f -> expectEqual "P18" "cdefg" (f "abcdefghijk" 3 7)) `mapM_` sliceFunctions

        let rotateFunctions = [rotate, rotate', rotate'2, rotate'3, rotate'4, rotate'5, rotate'6, rotate'7]
        (\f -> expectEqual "P19" "defghijkabc" (f "abcdefghijk" 3)) `mapM_` rotateFunctions

        let removeAtFunctions = [removeAt, removeAt', removeAt'2, removeAt'3, removeAt'4]
        (\f -> expectEqual "P20" ('b', "acd") (f 1 "abcd")) `mapM_` removeAtFunctions

        let insertAtFunctions = [insertAt, insertAt', insertAt'', insertAt''']
        (\f -> expectEqual "P21" ("a!bcd") (f '!' "abcd" 1)) `mapM_` insertAtFunctions

        return $ (Counts 0 0 0 0)

