module P80(
    Graph(..), Digraph(..), Edge(..),
    graphFromString, graphFromStringLabel,
    digraphFromString
) where

import Text.ParserCombinators.Parsec
import P70(getEither)


data Edge nodeType labelType = Edge nodeType nodeType labelType deriving(Eq, Show)
data Graph nodeType labelType = Graph [Edge nodeType labelType] deriving(Eq, Show)
data Digraph nodeType labelType = Digraph [Edge nodeType labelType] deriving(Eq, Show)

graphFromString :: String -> Graph Char ()
graphFromString input = Graph $ getEither $ parse edgeList "" input
    where edgeList :: Parser [Edge Char ()]
          edgeList =
            do char '['
               edges <- sepBy (try edge <|> oneNodeEdge) (string ", ")
               char ']'
               return edges
          edge =
            do n1 <- nodeValue
               char '-'
               n2 <- nodeValue
               return $ Edge n1 n2 ()
          oneNodeEdge =
            do n <- nodeValue
               return $ Edge n n ()
          nodeValue = noneOf "[]-,/"


graphFromStringLabel :: String -> Graph Char Int
graphFromStringLabel input = Graph $ getEither $ parse edgeList "" input
    where edgeList :: Parser [Edge Char Int]
          edgeList =
            do char '['
               edges <- sepBy (try edge <|> oneNodeEdge) (string ", ")
               char ']'
               return edges
          edge =
            do n1 <- nodeValue
               char '-'
               n2 <- nodeValue
               char '/'
               label <- many digit
               return $ Edge n1 n2 (read label)
          oneNodeEdge =
            do n <- nodeValue
               return $ Edge n n 0
          nodeValue = noneOf "[]-,/"

digraphFromString :: String -> Digraph Char ()
digraphFromString input = Digraph $ getEither $ parse edgeList "" input
    where edgeList :: Parser [Edge Char ()]
          edgeList =
            do char '['
               edges <- sepBy (try edge <|> oneNodeEdge) (string ", ")
               char ']'
               return edges
          edge =
            do n1 <- nodeValue
               char '-'
               n2 <- nodeValue
               return $ Edge n1 n2 ()
          oneNodeEdge =
            do n <- nodeValue
               return $ Edge n n ()
          nodeValue = noneOf "[]-,/"
