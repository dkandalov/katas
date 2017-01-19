import Test.HUnit
import Data.String.Utils

type Solution = [(Int, Int)]

solveForBoardOf :: Int -> Solution -> [Solution]
solveForBoardOf boardSize solution =
    if (length solution) == boardSize then [solution]
    else let column = length solution
         in [0..boardSize-1] >>= \row ->
            if isValidMove (row, column) solution
            then solveForBoardOf boardSize ((row, column) : solution)
            else []


isValidMove :: (Int, Int) -> Solution -> Bool
isValidMove queen solution =
    let notOnTheSameRow = \q1 q2 -> (fst q1) /= (fst q2)
        notInTheSameColumn = \q1 q2 -> (snd q1) /= (snd q2)
        notOnTheSameDiagonal = \q1 q2 -> (abs ((fst q1) - (fst q2))) /= (abs ((snd q1) - (snd q2)))
    in all (\it -> (notOnTheSameRow it queen) && (notInTheSameColumn it queen) && (notOnTheSameDiagonal it queen)) (filter (/= queen) solution)


asPrintableBoard :: Int -> Solution -> String
asPrintableBoard boardSize solution =
    let range = [0..boardSize-1]
        cellAsString = \pos -> if pos `elem` solution then "Q" else "x"
        rowAsString = \col -> join "" ((\row -> cellAsString (row, col)) `map` range)
    in
        join "\n" (rowAsString `map` range)

testIfMoveIsValid = TestList[
    "" ~: assertEqual "" True (isValidMove (0, 0) []),
    "" ~: assertEqual "" False (isValidMove (0, 0) [(0, 4)]),
    "" ~: assertEqual "" False (isValidMove (0, 0) [(4, 0)]),
    "" ~: assertEqual "" False (isValidMove (0, 0) [(4, 4)]),

    "" ~: assertEqual "" True (isValidMove (1, 1) [(2, 3)])
    ]

testConvertingSolutionIntoPrintableBoard = TestList[
    "" ~: (assertEqual "" "xxx\nxxx\nxxx" (asPrintableBoard 3 [])),
    "" ~: assertEqual "" "Qxx\nxxx\nxxx"(asPrintableBoard 3 [(0, 0)]),
    "" ~: assertEqual "" "xxx\nxQx\nxxQ"(asPrintableBoard 3 [(1, 1), (2, 2)])
    ]

-- this function is kind of broken, because for example (show "abc") converts it to string with quotes, i.e. "abc"
-- (wrote this just for fun)
myJoin :: (Show a) => String -> [a] -> String
myJoin separator [] = ""
myJoin separator [x] = show x
myJoin separator (x : xs) = (show x) ++ separator ++ (myJoin separator xs)

myJoinShouldCreateListFromList = TestList[
    "" ~: (assertEqual "" "" (myJoin "|" ([]::[Int]))),
    "" ~: (assertEqual "" "1" (myJoin "|" [1])),
    "" ~: (assertEqual "" "1|2" (myJoin "|" [1, 2])),
    "" ~: (assertEqual "" "1|2|3" (myJoin "|" [1, 2, 3]))
    ]


main = do
    putStrLn $ join "\n----\n" (asPrintableBoard 8 `map` (solveForBoardOf 8 []))
    runTestTT myJoinShouldCreateListFromList
    runTestTT testConvertingSolutionIntoPrintableBoard
    runTestTT testIfMoveIsValid
