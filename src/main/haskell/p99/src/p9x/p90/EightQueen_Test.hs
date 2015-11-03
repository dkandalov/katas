import Test.HUnit
import P70_Test(expectEqual, testList)
import P9x.P90.EightQueen

p90 = testList "P90" [
    expectEqual (Position 0 0) (next 3 (Position (-1) 2)),
    expectEqual (Position 0 1) (next 3 (Position 0 0)),
    expectEqual (Position 0 2) (next 3 (Position 0 1)),
    expectEqual (Position 1 0) (next 3 (Position 0 2)),

    expectEqual True (isValid $ boardToPositions ["Q"]),
    expectEqual True (isValid $ boardToPositions ["Q-", "--"]),
    expectEqual True (isValid $ boardToPositions ["--", "-Q"]),
    expectEqual False (isValid $ boardToPositions ["QQ", "--"]),
    expectEqual False (isValid $ boardToPositions ["Q-", "Q-"]),
    expectEqual False (isValid $ boardToPositions ["Q-", "-Q"]),
    expectEqual True (isValid $ boardToPositions ["Q--", "---", "-Q-"]),

    expectEqual [] (positionsToBoard 0 []),
    expectEqual ["Q"] (positionsToBoard 1 [Position 0 0]),
    expectEqual ["Q-", "--"] (positionsToBoard 2 [Position 0 0]),
    expectEqual ["-Q", "--"] (positionsToBoard 2 [Position 0 1]),
    expectEqual ["--", "Q-"] (positionsToBoard 2 [Position 1 0]),
    expectEqual ["--", "-Q"] (positionsToBoard 2 [Position 1 1]),
    expectEqual ["Q-", "-Q"] (positionsToBoard 2 [Position 0 0, Position 1 1]),

    expectEqual [] (boardToPositions []),
    expectEqual [Position 0 0] (boardToPositions ["Q"]),
    expectEqual [Position 0 0] (boardToPositions ["Q-", "--"]),
    expectEqual [Position 0 1] (boardToPositions ["-Q", "--"]),
    expectEqual [Position 1 0] (boardToPositions ["--", "Q-"]),
    expectEqual [Position 1 1] (boardToPositions ["--", "-Q"]),
    expectEqual [Position 0 0, Position 1 1] (boardToPositions ["Q-", "-Q"]),

    expectEqual [["Q"]]
                ((positionsToBoard 1) `map` (findQueenPositions 1)),
    expectEqual [] (findQueenPositions 2),
    expectEqual [] (findQueenPositions 3),
    expectEqual [[
                    "-Q--",
                    "---Q",
                    "Q---",
                    "--Q-"
                 ], [
                    "--Q-",
                    "Q---",
                    "---Q",
                    "-Q--"
                 ]] ((positionsToBoard 4) `map` (findQueenPositions 4)),
    expectEqual 10 (length $ findQueenPositions 5),
    expectEqual 92 (length $ findQueenPositions 8)
 ]

main :: IO Counts
main = runTestTT $ TestList[p90]