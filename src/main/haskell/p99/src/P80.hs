-- https://wiki.haskell.org/Multi-parameter_type_class
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module P80(
    Graph(..), Digraph(..), Edge(..),
    graphFromString, graphFromStringLabel,
    digraphFromString, digraphFromStringLabel,
    graphToTermForm, graphToAdjacentForm,
    digraphToTermForm, digraphToAdjacentForm,
    findPaths
) where

import Text.ParserCombinators.Parsec
import P70(getEither)
import Data.List

{-
-- not used, left here for reference
class GraphReader graphType nodeType labelType where
    fromString :: String -> graphType Char labelType
instance GraphReader Graph n () where
    fromString input = Graph $ getEither $ parse edges "" input
       where edges = edgeList (try edge <|> oneNodeEdgeUnit)
instance GraphReader Digraph n () where
    fromString input = Digraph $ getEither $ parse edges "" input
       where edges = edgeList (try edge <|> oneNodeEdgeUnit)

-}

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


-- P80

graphFromString :: String -> Graph Char ()
graphFromString input = Graph $ getEither $ parseEdges '-' input

graphFromStringLabel :: String -> Graph Char Int
graphFromStringLabel input = Graph $ getEither $ parseLabeledEdges '-' input

graphToTermForm :: Eq n => Graph n l -> ([n], [Edge n l])
graphToTermForm graph = toTermForm' (edges graph)

graphToAdjacentForm :: Eq n => Graph n l -> [(n, [n])]
graphToAdjacentForm graph = toAdjacentForm (edges graph)

digraphFromString :: String -> Digraph Char ()
digraphFromString input = Digraph $ getEither $ parseEdges '>' input

digraphFromStringLabel :: String -> Digraph Char Int
digraphFromStringLabel input = Digraph $ getEither $ parseLabeledEdges '>' input

digraphToTermForm :: Eq n => Digraph n l -> ([n], [Edge n l])
digraphToTermForm graph = toTermForm' (directedEdges graph)

digraphToAdjacentForm :: Eq n => Digraph n l -> [(n, [n])]
digraphToAdjacentForm graph = toAdjacentForm (directedEdges graph)


parseEdges :: Char -> String -> Either ParseError [Edge Char ()]
parseEdges nodeSeparator input = parse edges "" input
    where edges = edgeList (try (edge nodeSeparator) <|> oneNodeEdgeUnit)

parseLabeledEdges :: Char -> String -> Either ParseError [Edge Char Int]
parseLabeledEdges nodeSeparator input = parse edges "" input
    where edges = edgeList (try (edgeWithLabel nodeSeparator) <|> oneNodeEdgeZero)

toTermForm' :: Eq n => [Edge n l] -> ([n], [Edge n l])
toTermForm' edges = (allNodes, connections)
    where allNodes = nub $ (\it -> [fromNode it, toNode it]) `concatMap` edges
          connections = (\it -> (fromNode it) /= (toNode it)) `filter` edges

toAdjacentForm :: Eq n => [Edge n l] -> [(n, [n])]
toAdjacentForm edges = (\it -> (it, allNeighborsOf it)) `map` allNodes
    where allNodes = nub $ (\it -> [fromNode it, toNode it]) `concatMap` edges
          allNeighborsOf node = nub $ (neighborOf node) `concatMap` edges
          neighborOf node edge =
            if ((fromNode edge) == node && (toNode edge) == node) then []
            else if ((fromNode edge) == node) then [toNode edge]
            else if ((toNode edge) == node) then [fromNode edge]
            else []

edgeList :: Parser (Edge Char a) -> Parser [Edge Char a]
edgeList element = do
    char '['
    edges <- sepBy element (string ", ")
    char ']'
    return edges

edge :: Char -> Parser (Edge Char ())
edge nodeSeparator = do
    n1 <- nodeValue
    char nodeSeparator
    n2 <- nodeValue
    return $ Edge n1 n2 ()

oneNodeEdgeUnit = oneNodeEdge ()
oneNodeEdgeZero = oneNodeEdge 0

oneNodeEdge :: a -> Parser (Edge Char a)
oneNodeEdge defaultValue = do
    n <- nodeValue
    return $ Edge n n defaultValue

edgeWithLabel :: Char -> Parser (Edge Char Int)
edgeWithLabel nodeSeparator = do
    n1 <- nodeValue
    char nodeSeparator
    n2 <- nodeValue
    char '/'
    label <- many digit
    return $ Edge n1 n2 (read label)

nodeValue = noneOf "[]-,/"


-- P81
findPaths :: Eq n => n -> n -> Digraph n l -> [[n]]
findPaths from to graph = findPaths' from to graph []

findPaths' :: Eq n => n -> n -> Digraph n l -> [n] -> [[n]]
findPaths' from to graph path =
    if (from == to && not (null path)) then [path ++ [to]]
    else if (elem from path) then [[]]
    else (\node -> findPaths' node to graph (path ++ [from])) `concatMap` (neighborsOf from)
    where neighborsOf node =
            (\edge -> toNode edge) `map`
            ((\edge -> from == (fromNode edge) && (fromNode edge) /= (toNode edge)) `filter`
            (directedEdges graph))

