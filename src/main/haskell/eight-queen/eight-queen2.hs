import Test.HUnit
import Data.String.Utils

type Queen = (Int, Int)
type Solution = [Queen]

solve :: Int -> Solution -> [Solution]
solve boardSize solution
    | (length solution == boardSize) = [solution]
    | otherwise =
        [0..boardSize - 1] >>= (\row ->
          if not (isValidMove (row, column) solution) then []
          else solve boardSize ((row, column) : solution)
        )
        where column = length solution


isValidMove :: Queen -> Solution -> Bool
isValidMove (row, col) solution =
    let notOnTheSameRowOrColumn = \(thatRow, thatCol) -> thatRow /= row && thatCol /= col
        notOnTheSameDiagonal = \(thatRow, thatCol) -> abs (thatRow - row) /= abs (thatCol - col)
        isValid = \it -> (notOnTheSameRowOrColumn it) && (notOnTheSameDiagonal it)
    in all isValid solution


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

shouldFindPositionsForQueens = test [
     "" ~: assertEqual "" 0 (length (solve 2 [])),
     "" ~: assertEqual "" 0 (length (solve 3 [])),
     "" ~: assertEqual "" 2 (length (solve 4 [])),
     "" ~: assertEqual "" 10 (length (solve 5 [])),
     "" ~: assertEqual "" 92 (length (solve 8 []))
     ]

main =
    do
        putStrLn ("\n======\n" `join` ((asBoard 4) `map` (solve 4 [])))
        putStrLn ("\n======\n" `join` ((asBoard 5) `map` (solve 5 [])))
        putStrLn ("\n======\n" `join` ((asBoard 8) `map` (solve 8 [])))
        runTestTT shouldConvertSolutionIntoPrintableBoard
        runTestTT shouldDetermineIfMoveIsValid
        runTestTT shouldFindPositionsForQueens
