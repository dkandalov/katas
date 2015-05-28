import Test.HUnit
import P00_

main :: IO Counts
main =
    do
        sequence $
            (\f -> runTestTT $ TestCase $ assertEqual "P01" 8 (f [1, 1, 2, 3, 5, 8])) `map`
            [myLast', myLast'', myLast''', myLast'''', myLast''''']

        sequence $
            (\f -> runTestTT $ TestCase $ assertEqual "P02" 5 (f [1, 1, 2, 3, 5, 8])) `map`
            [myButLast, myButLast', myButLast'', myButLast''', myButLast'''', lastbut1]
        runTestTT $ TestCase $ assertEqual "P02" (Just 5) (lastbut1safe [1, 1, 2, 3, 5, 8])
