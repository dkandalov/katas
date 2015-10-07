import Test.HUnit
import P70_Test(expectEqual, testList)
import P90

p90 = testList "P90" [
    expectEqual (0, 0) (next 3 (-1, 2)),
    expectEqual (0, 1) (next 3 (0, 0)),
    expectEqual (0, 2) (next 3 (0, 1)),
    expectEqual (1, 0) (next 3 (0, 2)),

    expectEqual True (isValid $ boardToPositions ["Q"]),
    expectEqual True (isValid $ boardToPositions ["Q-", "--"]),
    expectEqual True (isValid $ boardToPositions ["--", "-Q"]),
    expectEqual False (isValid $ boardToPositions ["QQ", "--"]),
    expectEqual False (isValid $ boardToPositions ["Q-", "Q-"]),
    expectEqual False (isValid $ boardToPositions ["Q-", "-Q"]),
    expectEqual True (isValid $ boardToPositions ["Q--", "---", "-Q-"]),

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
    expectEqual [(0,0), (1,1)] (boardToPositions ["Q-", "-Q"]),

    expectEqual [boardToPositions ["Q"]]
                (findQueenPositions 1),
    expectEqual [] (findQueenPositions 2),
    expectEqual [] (findQueenPositions 3),
    expectEqual [boardToPositions [
                    "--Q-",
                    "Q---",
                    "---Q",
                    "-Q--"
                 ],
                 boardToPositions [
                    "-Q--",
                    "---Q",
                    "Q---",
                    "--Q-"
                 ]] (findQueenPositions 4)
 ]

main :: IO Counts
main = runTestTT $ TestList[p90]