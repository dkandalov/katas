module P90 (
    findQueenPositions, positionsToBoard, boardToPositions, next, isValid
) where

import Data.List.Split
import qualified Data.Maybe as Maybe

findQueenPositions :: Int -> [[(Int, Int)]]
findQueenPositions boardSize =
    filter (\it -> length it == boardSize) (findQueenPositions' boardSize (0, 0))

findQueenPositions' :: Int -> (Int, Int) -> [[(Int, Int)]]
findQueenPositions' boardSize position =
    if (fst position >= boardSize) then [[]]
    else filter isValid positionsList
    where positionsList = subResult
          subResult = (findQueenPositions' boardSize (next boardSize position)) ++
                      (\it -> position : it) `map` (findQueenPositions' boardSize (next boardSize position))

isValid :: [(Int, Int)] -> Bool
isValid positions = all (\(row1, col1) -> all (\(row2, col2) ->
    (row1 /= row2 && col1 /= col2) && (abs (row1 - row2) /= abs (col1 - col2))
 ) (filter (\it -> it /= (row1, col1)) positions)) positions

next :: Int -> (Int, Int) -> (Int, Int)
next boardSize (row, col) =
    if (col == boardSize - 1) then (row + 1, 0) else (row, col + 1)


positionsToBoard :: Int -> [(Int, Int)] -> [String]
positionsToBoard boardSize positions =
    chunksOf boardSize cells
    where cells = [ createCell row col | row <- [0..(boardSize-1)], col <- [0..(boardSize-1)] ]
          createCell row col = if (elem (row, col) positions) then 'Q' else '-'

boardToPositions :: [String] -> [(Int, Int)]
boardToPositions board = boardToPositions' 0 board

boardToPositions' :: Int -> [String] -> [(Int, Int)]
boardToPositions' _ [] = []
boardToPositions' rowIndex board =
    (Maybe.catMaybes $ parseRow rowIndex 0 $ head board) ++ (boardToPositions' (rowIndex+1) $ tail board)
    where parseRow _ _ [] = [Nothing]
          parseRow rowIndex cellIndex row =
            (parseCell rowIndex cellIndex $ head row) : (parseRow rowIndex (cellIndex+1) $ tail row)
          parseCell rowIndex cellIndex 'Q' = Just (rowIndex, cellIndex)
          parseCell _ _ '-' = Nothing
          parseCell _ _ x = error $ "Unsupported board symbol: " ++ [x]
