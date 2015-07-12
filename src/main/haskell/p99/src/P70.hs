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
stringToMTree s = fst $ stringToMTree' s

stringToMTree' :: String -> (MTree Char, String)
stringToMTree' (x:xs) = (MNode x children, rest)
    where (children, rest) = consumeChildren xs
          consumeChildren ('^':s) = ([], s)
          consumeChildren s = (child : nextChildren, s'')
            where (child, s') = stringToMTree' s
                  (nextChildren, s'') = consumeChildren s'

toString :: MTree Char -> String
toString (MNode value children) = value : (concat (toString `map` children)) ++ "^"