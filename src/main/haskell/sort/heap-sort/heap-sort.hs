data Heap = Heap {value :: Int, left :: Heap, right :: Heap} | Empty deriving (Eq, Show)

heapWith value = Heap value Empty Empty

add :: Int -> Heap -> Heap
add value Empty = Heap value Empty Empty
add value (Heap x Empty Empty) =
    if value < x then Heap value (heapWith x) Empty
    else Heap x (heapWith value) Empty
add value (Heap x leftHeap Empty) =
    if value < x then Heap value leftHeap (heapWith x)
    else Heap x leftHeap (heapWith value)
add value (Heap x Empty rightHeap) =
    if value < x then Heap value (heapWith x) rightHeap
    else Heap x (heapWith value) rightHeap
--add value (Heap x (Heap leftX _ _) (Hep rightX _ _)) =
--    if value < x then
--        if value < leftX Heap value (heapWith x) rightHeap
--        else
--    else Heap x (heapWith value) rightHeap

main = do
    putStrLn $ show $ Empty
    putStrLn $ show $ Heap 123 Empty Empty
    putStrLn $ show $ add 2 Empty
    putStrLn $ show $ add 1 $ add 2 Empty
    putStrLn $ show $ add 0 $ add 1 $ add 2 Empty
    putStrLn $ show $ add 3 $ add 1 $ add 2 Empty
    putStrLn $ show $ add 4 $ add 3 $ add 1 $ add 2 Empty