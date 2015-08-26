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
    graphFindPaths, digraphFindPaths,
    graphFindCycles, digraphFindCycles,
    spanningTrees
) where

import Text.ParserCombinators.Parsec
import P70(getEither)
import P50(GShow(..))
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
} deriving (Eq)

data Digraph nodeType labelType = Digraph {
    directedEdges :: [Edge nodeType labelType]
} deriving (Eq, Show)

instance (GShow nodeType, GShow labelType) => Show (Graph nodeType labelType) where
    show graph = "[" ++ (intercalate ", " stringEdges) ++ "]"
        where stringEdges = (\edge -> (gShow (fromNode edge)) ++ "-" ++ (gShow (toNode edge))) `map` (edges graph)


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
toTermForm' edges = (allNodes edges, connections)
    where connections = (\it -> (fromNode it) /= (toNode it)) `filter` edges

toAdjacentForm :: Eq n => [Edge n l] -> [(n, [n])]
toAdjacentForm edges = (\it -> (it, allNeighborsOf it)) `map` (allNodes edges)
    where allNeighborsOf node = nub $ (neighborOf node) `concatMap` edges
          neighborOf node edge =
            if ((fromNode edge) == node && (toNode edge) == node) then []
            else if ((fromNode edge) == node) then [toNode edge]
            else if ((toNode edge) == node) then [fromNode edge]
            else []

allNodes :: Eq n => [Edge n l] -> [n]
allNodes edges = nub $ (\it -> [fromNode it, toNode it]) `concatMap` edges

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
graphFindPaths :: Eq n => n -> n -> Graph n l -> [[n]]
graphFindPaths from to graph = findPaths' from to graph []

digraphFindPaths :: Eq n => n -> n -> Digraph n l -> [[n]]
digraphFindPaths from to graph = findPaths'' from to graph []

-- TODO refactor findPaths' and findPaths''
findPaths' :: Eq n => n -> n -> Graph n l -> [n] -> [[n]]
findPaths' from to graph path =
    if (from == to && not (null path)) then [path ++ [to]]
    else if (elem from path) then [[]]
    else (\it -> not $ null it) `filter` ((\node -> findPaths' node to graph (path ++ [from])) `concatMap` (graphNeighborsOf from graph))

findPaths'' :: Eq n => n -> n -> Digraph n l -> [n] -> [[n]]
findPaths'' from to graph path =
    if (from == to && not (null path)) then [path ++ [to]]
    else if (elem from path) then [[]]
    else (\node -> findPaths'' node to graph (path ++ [from])) `concatMap` (digraphNeighborsOf from graph)


-- P82
graphFindCycles :: Eq n => n -> Graph n l -> [[n]]
graphFindCycles from graph = graphFindCycles' from graph []

graphFindCycles' :: Eq n => n -> Graph n l -> [n] -> [[n]]
graphFindCycles' from graph path =
    if (length path > 2 && from == head path) then [path ++ [from]]
    else if (length path > 0 && elem from path) then []
    else (\node -> graphFindCycles' node graph (path ++ [from])) `concatMap` (graphNeighborsOf from graph)


digraphFindCycles :: Eq n => n -> Digraph n l -> [[n]]
digraphFindCycles from graph = digraphFindCycles' from graph []

digraphFindCycles' :: Eq n => n -> Digraph n l -> [n] -> [[n]]
digraphFindCycles' from graph path =
    if (length path > 2 && from == head path) then [path ++ [from]]
    else if (length path > 0 && elem from path) then []
    else (\node -> digraphFindCycles' node graph (path ++ [from])) `concatMap` (digraphNeighborsOf from graph)


graphNeighborsOf :: Eq n => n -> Graph n l -> [n]
graphNeighborsOf node graph =
    (\edge -> if (node == fromNode edge) then toNode edge else fromNode edge) `map`
    ((\edge ->
        (fromNode edge) /= (toNode edge) &&
        (node == fromNode edge || node == toNode edge)
    ) `filter`
    (edges graph))

digraphNeighborsOf :: Eq n => n -> Digraph n l -> [n]
digraphNeighborsOf node graph =
    (\edge -> toNode edge) `map`
    ((\edge -> node == (fromNode edge) && (fromNode edge) /= (toNode edge)) `filter`
    (directedEdges graph))


-- P83
spanningTrees :: (Ord n, Eq n, Eq l) => Graph n l -> [Graph n l]
spanningTrees graph = spanningTrees' graph []

spanningTrees' :: (Ord n, Eq n, Eq l) => Graph n l -> [Edge n l] -> [Graph n l]
spanningTrees' graph path = (\path -> Graph path) `map`
                            (unique $
                            (\node -> spanningTreesFrom node (edges graph) allNodesInGraph) `concatMap`
                            allNodesInGraph)
    where allNodesInGraph = allNodes $ edges graph
          unique spanningTrees = nub $ ordered `map` spanningTrees
          ordered edges = Data.List.sortBy compareEdges edges
          compareEdges edge1 edge2 =
            case (compare (fromNode edge1) (fromNode edge2)) of
                EQ -> compare (toNode edge1) (toNode edge2)
                result@_ -> result

spanningTreesFrom :: (Ord n, Eq n, Eq l) => n -> [Edge n l] -> [n] -> [[Edge n l]]
spanningTreesFrom node edgeList nodes =
    if (not $ elem node nodes) then []
    else if (null remaningNodes) then [[]]
    else if (null edgeList) then []
    else result
    where result = subResult `concatMap` edgesWithNode
          subResult edge =
            (\it -> edge : it) `map` (spanningTreesFrom (other node edge) edgesWithoutNode remaningNodes)
          remaningNodes = Data.List.delete node nodes
          edgesWithNode = (hasNode node) `filter` edgeList
          edgesWithoutNode = (not . (hasNode node)) `filter` edgeList
          hasNode node edge = fromNode edge == node || toNode edge == node
          other node edge = if (fromNode edge == node) then toNode edge else fromNode edge
