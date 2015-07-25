module P80(
) where

import qualified Data.Map as Map

data Edge a b = Edge (Node b a) (Node b a) a
data Node a b = Node a [Edge b a]
data Graph a b = Graph (Map.Map a [Node a b]) [Edge b a]

toTuple :: Edge a b -> (b, b, a)
toTuple (Edge (Node value1 _) (Node value2 _) edgeValue) = (value1, value2, edgeValue)

equal :: Graph a b -> Graph a b -> Bool
equal g1 g2 = False