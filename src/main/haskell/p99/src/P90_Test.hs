import Test.HUnit
import P70_Test(expectEqual, testList)
import P90

p90 = testList "P90" [
    expectEqual [] (positionsToBoard 0 []),
    expectEqual ["Q"] (positionsToBoard 1 [(0, 0)]),
    expectEqual ["Q-", "--"] (positionsToBoard 2 [(0, 0)]),
    expectEqual ["-Q", "--"] (positionsToBoard 2 [(0, 1)]),
    expectEqual ["--", "Q-"] (positionsToBoard 2 [(1, 0)]),
    expectEqual ["--", "-Q"] (positionsToBoard 2 [(1, 1)]),
    expectEqual ["Q-", "-Q"] (positionsToBoard 2 [(0, 0), (1, 1)])

--    expectEqual [(0, 0)] (findQueenPositions 1),
--    expectEqual [] (findQueenPositions 2)
 ]

main :: IO Counts
main = runTestTT $ TestList[p90]