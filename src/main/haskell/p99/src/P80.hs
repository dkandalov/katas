module P80(
) where

import qualified Data.Map as Map

-- Edge
data Edge u t = Edge (Node t u) (Node t u) u

toTuple :: Edge u t -> (t, t, u)
toTuple (Edge (Node value1 _) (Node value2 _) edgeValue) = (value1, value2, edgeValue)

-- Node
data Node t u = Node t [Edge u t]

neighbours :: Node t u -> [Node t u]
neighbours _ = []

-- Graph
data Graph t u = Graph (Map.Map t [Node t u]) [Edge u t]

edgeTarget :: Edge u t -> Node t u -> Maybe (Node t u)
edgeTarget _ node = Just node

equal :: Graph t u -> Graph t u -> Bool
equal g1 g2 = False

addNode :: Graph a b -> a -> Graph a b
addNode graph _ = graph
