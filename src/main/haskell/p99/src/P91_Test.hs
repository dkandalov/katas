import Test.HUnit
import P70_Test(expectEqual, testList)
import P91

p91 = testList "P91" [
     expectEqual [] (findKnightTours 0),
     expectEqual [[(0, 0)]] (findKnightTours 1),
     expectEqual [] (findKnightTours 2),
     expectEqual [] (findKnightTours 3),
     expectEqual [] (findKnightTours 4),
     expectEqual [
        (4,4),(2,3),(0,4),(1,2),(2,4),
        (0,3),(1,1),(3,0),(4,2),(3,4),
        (1,3),(0,1),(2,2),(4,3),(3,1),
        (1,0),(0,2),(1,4),(3,3),(4,1),
        (2,0),(3,2),(4,0),(2,1),(0,0)
     ] (head $ findKnightTours 5)
--     expectEqual 1728 (length $ findKnightTours 5)
 ]

main :: IO Counts
main = runTestTT $ TestList[p91]