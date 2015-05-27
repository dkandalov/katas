import Test.HUnit
import P00_

main :: IO Counts
main =
    do
        runTestTT $ TestCase $ assertEqual "P01" 8 (myLast' [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P01" 8 (myLast'' [1, 1, 2, 3, 5, 8])
        runTestTT $ TestCase $ assertEqual "P01" 8 (myLast''' [1, 1, 2, 3, 5, 8])

