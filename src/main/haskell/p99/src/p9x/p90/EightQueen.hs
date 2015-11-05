module P9x.P90.EightQueen (
    findQueenPositions,
    positionsToBoard,
    boardToPositions,
    next,
    isValid,
    Position(..)
) where

import Data.List.Split
import qualified Data.Maybe as Maybe

findQueenPositions :: Int -> [[Position]]
findQueenPositions boardSize =
    filter (\it -> length it == boardSize) (findQueenPositions' boardSize (Position 0 0) [])

findQueenPositions' :: Int -> Position -> [Position] -> [[Position]]
findQueenPositions' boardSize position positions =
    if (row position == boardSize) then [positions]
    else if (isValid newPositions) then
        subResultWithPosition ++ subResultWithoutPosition else subResultWithoutPosition
    where newPositions = position : positions
          subResultWithPosition = findQueenPositions' boardSize (next boardSize position) newPositions
          subResultWithoutPosition = findQueenPositions' boardSize (next boardSize position) positions

isValid :: [Position] -> Bool
isValid positions = all (\p1 -> all (\p2 ->
    p1 == p2 || not (onTheSameRow p1 p2 || onTheSameDiagonal p1 p2)
 ) positions) positions

next :: Int -> Position -> Position
next boardSize p =
    if (column p == boardSize - 1) then nextRow p else nextColumn p
    where nextRow p = Position ((row p) + 1) 0
          nextColumn p = Position (row p) ((column p) + 1)


positionsToBoard :: Int -> [Position] -> [String]
positionsToBoard boardSize positions =
    chunksOf boardSize cells
    where cells = [ createCell row col | row <- [0..(boardSize-1)], col <- [0..(boardSize-1)] ]
          createCell row col = if (elem (Position row col) positions) then 'Q' else '-'

boardToPositions :: [String] -> [Position]
boardToPositions board = boardToPositions' 0 board

boardToPositions' :: Int -> [String] -> [Position]
boardToPositions' _ [] = []
boardToPositions' rowIndex board =
    (Maybe.catMaybes $ parseRow rowIndex 0 $ head board) ++ (boardToPositions' (rowIndex+1) $ tail board)
    where parseRow _ _ [] = [Nothing]
          parseRow rowIndex cellIndex row =
            (parseCell rowIndex cellIndex $ head row) : (parseRow rowIndex (cellIndex+1) $ tail row)
          parseCell rowIndex cellIndex 'Q' = Just (Position rowIndex cellIndex)
          parseCell _ _ '-' = Nothing
          parseCell _ _ x = error $ "Unsupported board symbol: " ++ [x]


data Position = Position { row :: Int, column :: Int } deriving (Eq, Show)
onTheSameRow :: Position -> Position -> Bool
onTheSameRow p1 p2 = (row p1 == row p2) || (column p1 == column p2)
onTheSameDiagonal :: Position -> Position -> Bool
onTheSameDiagonal p1 p2 = (abs $ (row p1) - (row p2)) == (abs $ (column p1) - (column p2))
