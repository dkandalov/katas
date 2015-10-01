module P90 (
    findQueenPositions, positionsToBoard, boardToPositions
) where

import Data.List.Split
import qualified Data.Maybe as Maybe

findQueenPositions :: Int -> [(Int, Int)]
findQueenPositions boardSize = [(0, 0)]

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
