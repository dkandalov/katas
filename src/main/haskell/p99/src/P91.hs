module P91(
    findKnightTours
) where

findKnightTours :: Int -> [[(Int, Int)]]
findKnightTours 0 = []
findKnightTours 1 = [[(0,0)]]
findKnightTours boardSize = findKnightTours' boardSize (0, 0) [(0, 0)]

findKnightTours' :: Int -> (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
findKnightTours' boardSize position moves =
    if (length moves == boardSize * boardSize) then [moves]
    else (\it -> findKnightTours' boardSize it (it : moves)) `concatMap` nextValidMoves
    where nextMoves = knightMoves position
          nextValidMoves = notVisited `filter` (isOnBoard `filter` nextMoves)
          isOnBoard (row, column) =
            row >= 0 && row < boardSize && column >= 0 && column < boardSize
          notVisited pos = not (elem pos moves)

knightMoves :: (Int, Int) -> [(Int, Int)]
knightMoves position = [
    (row - 1, column - 2),
    (row + 1, column - 2),
    (row + 2, column - 1),
    (row + 2, column + 1),
    (row + 1, column + 2),
    (row - 1, column + 2),
    (row - 2, column + 1),
    (row - 2, column - 1)
 ]
 where row = fst position
       column = snd position