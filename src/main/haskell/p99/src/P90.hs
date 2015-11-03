module P90 (
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
isValid positions = all (\(Position row1 col1) -> all (\(Position row2 col2) ->
    (row1 /= row2 && col1 /= col2) && (abs (row1 - row2) /= abs (col1 - col2))
 ) (filter (\it -> it /= (Position row1 col1)) positions)) positions

next :: Int -> Position -> Position
next boardSize (Position row col) =
    if (col == boardSize - 1) then (Position (row + 1) 0) else (Position row (col + 1))


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