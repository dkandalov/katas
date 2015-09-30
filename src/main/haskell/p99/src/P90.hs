module P90 (
    findQueenPositions, positionsToBoard
) where

import Data.List.Split

findQueenPositions :: Int -> [(Int, Int)]
findQueenPositions boardSize = [(0, 0)]

positionsToBoard :: Int -> [(Int, Int)] -> [String]
positionsToBoard boardSize positions =
    chunksOf boardSize cells
    where cells = [ createCell row col | row <- [0..(boardSize-1)], col <- [0..(boardSize-1)] ]
          createCell row col = if (elem (row, col) positions) then 'Q' else '-'