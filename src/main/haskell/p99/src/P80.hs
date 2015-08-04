module P80(
    Graph(..), Digraph(..), Edge(..),
    graphFromString, graphFromStringLabel,
    digraphFromString, digraphFromStringLabel
) where

import Text.ParserCombinators.Parsec
import P70(getEither)


data Edge nodeType labelType = Edge nodeType nodeType labelType deriving(Eq, Show)
data Graph nodeType labelType = Graph [Edge nodeType labelType] deriving(Eq, Show)
data Digraph nodeType labelType = Digraph [Edge nodeType labelType] deriving(Eq, Show)

graphFromString :: String -> Graph Char ()
graphFromString input = Graph $ getEither $ parse edges "" input
    where edges = edgeList (try edge <|> oneNodeEdgeUnit)

graphFromStringLabel :: String -> Graph Char Int
graphFromStringLabel input = Graph $ getEither $ parse edges "" input
    where edges = edgeList (try edgeWithLabel <|> oneNodeEdgeZero)

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
