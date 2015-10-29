import Test.HUnit
import P70_Test(expectEqual, testList)
import P90

p90 = testList "P91" [
     expectEqual [] (findKnightTours 0),
     expectEqual [(0, 0)] (findKnightTours 1)
 ]

-- can't really run, get "Something is amiss"