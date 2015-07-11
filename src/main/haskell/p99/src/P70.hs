module P70(
    MTree(..),
    nodeCount,
    stringToMTree, toString
) where

data MTree a = MNode a [MTree a] deriving (Show, Eq)

-- P70
nodeCount :: MTree a -> Int
nodeCount (MNode _ children) = 1 + (sum (nodeCount `map` children))

stringToMTree :: String -> MTree Char
stringToMTree _ = MNode ' ' []

toString :: MTree Char -> String
toString (MNode value children) = value : (concat (toString `map` children)) ++ "^"