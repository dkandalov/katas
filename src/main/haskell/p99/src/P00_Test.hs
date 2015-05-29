import Test.HUnit
import P00_

runTest = runTestTT . TestCase

main :: IO Counts
main = do
        (\f -> runTest $ assertEqual "P01" 8 (f [1, 1, 2, 3, 5, 8])) `mapM_`
            [myLast', myLast'', myLast''', myLast'''', myLast''''']

        (\f -> runTest $ assertEqual "P02" 5 (f [1, 1, 2, 3, 5, 8])) `mapM_`
            [myButLast, myButLast', myButLast'', myButLast''', myButLast'''', lastbut1]

        runTest $ assertEqual "P02" (Just 5) (lastbut1safe [1, 1, 2, 3, 5, 8])

        (\f -> runTest $ assertEqual "P03" 2 (f 2 [1, 1, 2, 3, 5, 8])) `mapM_`
            [elementAt, elementAt', elementAt''{-, elementAt'''-}]

        return $ (Counts 0 0 0 0)

