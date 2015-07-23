module P70(
    MTree(..),
    nodeCount,
    stringToMTree, toString, parseAsMTree,
    internalPathLength,
    postorder,
    toLispyTree, fromLispyTree, parseAsLispyTree
) where

import P50(GShow(..)) -- don't really need this, left it here to check importing class instances
import Text.ParserCombinators.Parsec

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

parseAsMTree :: String -> MTree Char
parseAsMTree input = getEither $ parse node "" input
    where node :: Parser (MTree Char)
          node =
              do name <- noneOf "^"
                 children <- many node
                 char '^'
                 return (MNode name children)

getEither :: (Show left) => (Either left right) -> right
getEither e = case e of
    Left parseError -> error $ show parseError
    Right value -> value

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
    where (children, xs') = readNodes xs
          rest = tail xs' -- drop ')'
readNode (x:' ':xs) = (Just (MNode x []), xs)
readNode (x:xs) = (Just (MNode x []), xs)

readNodes :: String -> ([MTree Char], String)
readNodes xs@(')':_) = ([], xs)
readNodes (' ':xs) = readNodes xs
readNodes xs = (nodes, xs'')
    where nodes = case mayBeNode of
            Nothing -> []
            Just node -> node : nextNodes
          (mayBeNode, xs') = readNode xs
          (nextNodes, xs'') = readNodes xs'

parseAsLispyTree :: String -> MTree Char
parseAsLispyTree input = getEither $ parse node "" input
    where node :: Parser (MTree Char)
          node = leafNode <|> internalNode
          leafNode =
            do name <- nodeName
               return $ MNode name []
          internalNode =
            do char '('
               name <- nodeName
               children <- many $ (char ' ') >> node
               char ')'
               return $ MNode name children
          nodeName = noneOf "() "

