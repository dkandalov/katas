-- example from http://learnyouahaskell.com/a-fistful-of-monads
import Test.HUnit
import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (col, row) = do
    (newCol, newRow) <- [
        (col + 2, row - 1), (col + 2, row + 1), (col - 2, row - 1), (col - 2, row + 1),
        (col + 1, row - 2), (col + 1, row + 2), (col - 1, row - 2), (col - 1, row + 2)]
    guard (newCol `elem` [1..8] && newRow `elem` [1..8])
    return (newCol, newRow)

in3 :: KnightPos -> [KnightPos]
in3 fromPos = return fromPos >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 fromPos toPos = toPos `elem` in3 fromPos

shouldFindAllValidNextMoves = test [
    "" ~: assertEqual "" [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)] (moveKnight (6, 2)),
    "" ~: assertEqual "" [(6,2),(7,3)] (moveKnight (8, 1))
    ]

shouldFindPositionsReachableInThreeMoves = test [
    "" ~: assertEqual "" True ((6,2) `canReachIn3` (6,1)),
    "" ~: assertEqual "" False ((6,2) `canReachIn3` (7, 3))
    ]

main = do
    runTestTT shouldFindAllValidNextMoves
    runTestTT shouldFindPositionsReachableInThreeMoves
