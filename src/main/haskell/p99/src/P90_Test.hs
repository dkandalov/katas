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
    expectEqual ["Q-", "-Q"] (positionsToBoard 2 [(0, 0), (1, 1)]),

    expectEqual [] (boardToPositions []),
    expectEqual [(0,0)] (boardToPositions ["Q"]),
    expectEqual [(0,0)] (boardToPositions ["Q-", "--"]),
    expectEqual [(0,1)] (boardToPositions ["-Q", "--"]),
    expectEqual [(1,0)] (boardToPositions ["--", "Q-"]),
    expectEqual [(1,1)] (boardToPositions ["--", "-Q"]),
    expectEqual [(0,0), (1,1)] (boardToPositions ["Q-", "-Q"])

--    expectEqual ["Q"] (positionsToBoard 1 $ findQueenPositions 1),
--    expectEqual [] (positionsToBoard 2 $ findQueenPositions 2),
--    expectEqual [] (positionsToBoard 3 $ findQueenPositions 3)
 ]

main :: IO Counts
main = runTestTT $ TestList[p90]