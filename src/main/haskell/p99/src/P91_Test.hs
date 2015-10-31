import Test.HUnit
import P70_Test(expectEqual, testList)
import P91

p91 = testList "P91" [
     expectEqual [] (findKnightTours 0),
     expectEqual [[(0, 0)]] (findKnightTours 1),
     expectEqual [] (findKnightTours 2),
     expectEqual [] (findKnightTours 3)
 ]

main :: IO Counts
main = runTestTT $ TestList[p91]