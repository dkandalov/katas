import Test.HUnit

type Solution = [(Int, Int)]

solveForBoardOf :: Int -> [Solution]
solveForBoardOf size = []

asPrintableBoard :: Int -> Solution -> String
asPrintableBoard boardSize solution = ""


tests = TestList[
    "" ~: (assertEqual "" "xxx\nxxx\nxxx" (asPrintableBoard 3 []))
--    "" ~: assertEqual "" (asPrintableBoard 3 [(0, 0)]) ""
    ]

main =
    putStr $ unlines $ (\row -> (\col -> "x") `map` [1..3]) `map` [1..3]
--    runTestTT tests