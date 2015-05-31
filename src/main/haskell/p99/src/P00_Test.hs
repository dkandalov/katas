import Test.HUnit
import P00_

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

        return $ (Counts 0 0 0 0)

