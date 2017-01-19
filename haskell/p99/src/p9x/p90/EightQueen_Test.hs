import Test.HUnit
import P9x.Util(expectEqual_, testList)
import P9x.P90.EightQueen

p90 = testList "P90" [
    expectEqual_ (Position 0 0) (next 3 (Position (-1) 2)),
    expectEqual_ (Position 0 1) (next 3 (Position 0 0)),
    expectEqual_ (Position 0 2) (next 3 (Position 0 1)),
    expectEqual_ (Position 1 0) (next 3 (Position 0 2)),

    expectEqual_ True (isValid $ boardToPositions ["Q"]),
    expectEqual_ True (isValid $ boardToPositions ["Q-", "--"]),
    expectEqual_ True (isValid $ boardToPositions ["--", "-Q"]),
    expectEqual_ False (isValid $ boardToPositions ["QQ", "--"]),
    expectEqual_ False (isValid $ boardToPositions ["Q-", "Q-"]),
    expectEqual_ False (isValid $ boardToPositions ["Q-", "-Q"]),
    expectEqual_ True (isValid $ boardToPositions ["Q--", "---", "-Q-"]),

    expectEqual_ [] (positionsToBoard 0 []),
    expectEqual_ ["Q"] (positionsToBoard 1 [Position 0 0]),
    expectEqual_ ["Q-", "--"] (positionsToBoard 2 [Position 0 0]),
    expectEqual_ ["-Q", "--"] (positionsToBoard 2 [Position 0 1]),
    expectEqual_ ["--", "Q-"] (positionsToBoard 2 [Position 1 0]),
    expectEqual_ ["--", "-Q"] (positionsToBoard 2 [Position 1 1]),
    expectEqual_ ["Q-", "-Q"] (positionsToBoard 2 [Position 0 0, Position 1 1]),

    expectEqual_ [] (boardToPositions []),
    expectEqual_ [Position 0 0] (boardToPositions ["Q"]),
    expectEqual_ [Position 0 0] (boardToPositions ["Q-", "--"]),
    expectEqual_ [Position 0 1] (boardToPositions ["-Q", "--"]),
    expectEqual_ [Position 1 0] (boardToPositions ["--", "Q-"]),
    expectEqual_ [Position 1 1] (boardToPositions ["--", "-Q"]),
    expectEqual_ [Position 0 0, Position 1 1] (boardToPositions ["Q-", "-Q"]),

    expectEqual_ [["Q"]]
                ((positionsToBoard 1) `map` (findQueenPositions 1)),
    expectEqual_ [] (findQueenPositions 2),
    expectEqual_ [] (findQueenPositions 3),
    expectEqual_ [[
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
    expectEqual_ 10 (length $ findQueenPositions 5),
    expectEqual_ 92 (length $ findQueenPositions 8)
 ]

main :: IO Counts
main = runTestTT $ TestList[p90]