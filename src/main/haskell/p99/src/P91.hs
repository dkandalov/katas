module P91(
    findKnightTours
) where

findKnightTours :: Int -> [(Int, Int)]
findKnightTours 0 = []
findKnightTours 1 = [(0,0)]