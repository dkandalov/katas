module P80(
    Graph(..), Digraph(..), Edge(..),
    graphFromString, graphFromStringLabel,
    digraphFromString, digraphFromStringLabel,
    toTermForm, toAdjacentForm
) where

import Text.ParserCombinators.Parsec
import P70(getEither)
import Data.List


data Edge nodeType labelType = Edge {
    fromNode :: nodeType,
    toNode:: nodeType,
    label :: labelType
} deriving(Eq, Show)

data Graph nodeType labelType = Graph {
    edges :: [Edge nodeType labelType]
} deriving(Eq, Show)

data Digraph nodeType labelType = Digraph {
    directedEdges :: [Edge nodeType labelType]
} deriving(Eq, Show)



graphFromString :: String -> Graph Char ()
graphFromString input = Graph $ getEither $ parse edges "" input
    where edges = edgeList (try edge <|> oneNodeEdgeUnit)

graphFromStringLabel :: String -> Graph Char Int
graphFromStringLabel input = Graph $ getEither $ parse edges "" input
    where edges = edgeList (try edgeWithLabel <|> oneNodeEdgeZero)

toTermForm :: Eq n => Graph n l -> ([n], [Edge n l])
toTermForm graph = (allNodes, connections)
    where allNodes = nub $ (\it -> [fromNode it, toNode it]) `concatMap` (edges graph)
          connections = (\it -> (fromNode it) /= (toNode it)) `filter` (edges graph)

toAdjacentForm :: Eq n => Graph n l -> [(n, [n])]
toAdjacentForm graph = (\it -> (it, allNeighborsOf it)) `map` allNodes
    where allNodes = nub $ (\it -> [fromNode it, toNode it]) `concatMap` (edges graph)
          allNeighborsOf node = nub $ (neighborOf node) `concatMap` (edges graph)
          neighborOf node edge =
            if ((fromNode edge) == node && (toNode edge) == node) then []
            else if ((fromNode edge) == node) then [toNode edge]
            else if ((toNode edge) == node) then [fromNode edge]
            else []


digraphFromString :: String -> Digraph Char ()
digraphFromString input = Digraph $ getEither $ parse edges "" input
    where edges = edgeList (try edge <|> oneNodeEdgeUnit)

digraphFromStringLabel :: String -> Digraph Char Int
digraphFromStringLabel input = Digraph $ getEither $ parse edges "" input
    where edges = edgeList (try edgeWithLabel <|> oneNodeEdgeZero)


edgeList :: Parser (Edge Char a) -> Parser [Edge Char a]
edgeList element = do
    char '['
    edges <- sepBy element (string ", ")
    char ']'
    return edges

edge :: Parser (Edge Char ())
edge = do
    n1 <- nodeValue
    char '-'
    n2 <- nodeValue
    return $ Edge n1 n2 ()

oneNodeEdgeUnit = oneNodeEdge ()
oneNodeEdgeZero = oneNodeEdge 0

oneNodeEdge :: a -> Parser (Edge Char a)
oneNodeEdge defaultValue = do
    n <- nodeValue
    return $ Edge n n defaultValue

edgeWithLabel :: Parser (Edge Char Int)
edgeWithLabel = do
    n1 <- nodeValue
    char '-'
    n2 <- nodeValue
    char '/'
    label <- many digit
    return $ Edge n1 n2 (read label)

nodeValue = noneOf "[]-,/"
