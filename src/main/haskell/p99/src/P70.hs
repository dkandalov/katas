module P70(
    MTree(..),
    nodeCount,
    stringToMTree, toString,
    internalPathLength,
    postorder,
    lispyTree
) where

import P50(GShow(..)) -- don't really need this, left it here to check importing class instances

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


-- P71
internalPathLength :: MTree a -> Int
internalPathLength tree = internalPathLength' 0 tree
    where internalPathLength' depth (MNode _ children) =
            depth + (sum $ (internalPathLength' (depth + 1)) `map` children)


-- P72
postorder :: MTree a -> [a]
postorder (MNode value children) = (postorder `concatMap` children) ++ [value]


-- P73
lispyTree :: GShow a => MTree a -> String
lispyTree (MNode value children) =
    if (null children) then (gShow value)
    else "(" ++ (gShow value) ++ " " ++ childrenAsString ++ ")"
    where childrenAsString = unwords (lispyTree `map` children)