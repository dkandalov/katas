import Test.HUnit
import Data.String.Utils

type Queen = (Int, Int)
type Solution = [Queen]

solve :: Int -> Solution -> [Solution]
solve boardSize solution =
    let column = length solution
    in
        if column == boardSize then [solution]
        else [0..boardSize - 1] >>= (\row ->
            if isValidMove (row, column) solution then solve boardSize ((row, column) : solution)
            else []
        )


isValidMove :: Queen -> Solution -> Bool
isValidMove (row, col) solution =
    let notOnTheSameRowOrColumn = \queen -> (fst queen) /= row && (snd queen) /= col
        notOnTheSameDiagonal = \queen -> abs ((fst queen) - row) /= abs ((snd queen) - col)
    in all (\it -> (notOnTheSameRowOrColumn it) && (notOnTheSameDiagonal it)) (filter (/= (row, col)) solution)


asBoard :: Int -> Solution -> String
asBoard boardSize solution =
    let asChar = \queen -> if queen `elem` solution then "Q" else "-"
        rowAsString = \row ->  [0..boardSize - 1] >>= (\col -> asChar (row, col))
    in "\n" `join`(rowAsString `map` [0..boardSize - 1])


shouldConvertSolutionIntoPrintableBoard = test [
    "" ~: assertEqual "" "---\n---\n---" (asBoard 3 []),
    "" ~: assertEqual "" "Q--\n---\n---" (asBoard 3 [(0, 0)]),
    "" ~: assertEqual "" "Q--\n-Q-\n--Q" (asBoard 3 [(0, 0), (1, 1), (2, 2)])
    ]

shouldDetermineIfMoveIsValid = test [
    "" ~: assertEqual "" True (isValidMove (0, 0) []),
    "" ~: assertEqual "" False (isValidMove (0, 0) [(0, 1)]),
    "" ~: assertEqual "" False (isValidMove (0, 0) [(1, 0)]),
    "" ~: assertEqual "" False (isValidMove (0, 0) [(1, 1)])
    ]


main =
    do
        putStrLn ("\n======\n" `join` ((asBoard 4) `map` (solve 4 [])))
        putStrLn ("\n======\n" `join` ((asBoard 5) `map` (solve 5 [])))
        putStrLn ("\n======\n" `join` ((asBoard 8) `map` (solve 8 [])))
        runTestTT shouldConvertSolutionIntoPrintableBoard
        runTestTT shouldDetermineIfMoveIsValid
