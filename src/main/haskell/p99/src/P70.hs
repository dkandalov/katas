module P70(
    MTree(..),
    nodeCount,
    stringToMTree, toString,
    internalPathLength,
    postorder,
    toLispyTree, fromLispyTree
) where

import P50(GShow(..)) -- don't really need this, left it here to check importing class instances

data MTree a = MNode a [MTree a] deriving (Show, Eq)

-- P70
nodeCount :: MTree a -> Int
nodeCount (MNode _ children) = 1 + (sum (nodeCount `map` children))

stringToMTree :: String -> MTree Char
stringToMTree s = fst $ stringToMTree' s -- TODO catch and report in nicer way

stringToMTree' :: String -> (MTree Char, String)
stringToMTree' [] = error "Failed to parse string as multiway tree"
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
toLispyTree :: GShow a => MTree a -> String
toLispyTree (MNode value children) =
    if (null children) then (gShow value)
    else "(" ++ (gShow value) ++ " " ++ childrenAsString ++ ")"
    where childrenAsString = unwords (toLispyTree `map` children)

fromLispyTree :: String -> MTree Char
fromLispyTree xs = case (fst $ readNode xs) of
    Nothing -> error ""
    Just node -> node

readNode :: String -> (Maybe (MTree Char), String)
readNode "" = (Nothing, "")
readNode ('(':x:' ':xs) = (Just (MNode x children), rest)
    where (children, rest) = readNodes xs
readNode (')':xs) = (Nothing, xs)
readNode (x:' ':xs) = (Just (MNode x []), xs)
readNode (x:xs) = (Just (MNode x []), xs)

readNodes :: String -> ([MTree Char], String)
readNodes xs = (nodes, xs'')
    where nodes = case mayBeNode of
            Nothing -> []
            Just node -> node : nextNodes
          (mayBeNode, xs') = readNode xs
          (nextNodes, xs'') = readNodes xs'