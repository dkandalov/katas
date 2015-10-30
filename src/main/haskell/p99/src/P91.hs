module P91(
    findKnightTours
) where

findKnightTours :: Int -> [(Int, Int)]
findKnightTours 0 = []
findKnightTours 1 = [(0,0)]
findKnightTours boardSize = findKnightTours' boardSize (0, 0)

findKnightTours' :: Int -> (Int, Int) -> [(Int, Int)]
findKnightTours' boardSize position = []

knightMoves :: (Int, Int) -> [(Int, Int)]
knightMoves position = [
    ((fst position) - 1, (snd position) - 2),
    ((fst position) + 1, (snd position) - 2),
    ((fst position) + 2, (snd position) - 1),
    ((fst position) + 2, (snd position) + 1),
    ((fst position) + 1, (snd position) + 2),
    ((fst position) - 1, (snd position) + 2),
    ((fst position) - 2, (snd position) + 1),
    ((fst position) - 2, (snd position) - 1)
 ]