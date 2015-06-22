module P50 (
    Tree(..),
    cBalanced
) where

data Tree a = Node { value :: a, left :: Tree a, right :: Tree a } | End
              deriving (Eq)
leafNode :: a -> Tree a
leafNode value = Node value End End

instance (Show a) => Show (Tree a) where
    show End = "."
    show (Node value End End) = "T(" ++ (show value) ++ ")"
    show (Node value left right) =
            "T(" ++ (show value) ++ "," ++
            (show left) ++ "," ++ (show right) ++ ")"

-- P55
cBalanced :: Int -> a -> [Tree a]
cBalanced 0 _ = []
cBalanced 1 value = [leafNode value]
cBalanced 2 value = [(Node value (leafNode value) End), (Node value End (leafNode value))]
cBalanced 3 value = [(Node value (leafNode value) (leafNode value))]
cBalanced n value =
    if ((n - 1) `rem` 2 == 0) then
        nodeVariations balancedHalf balancedHalf
    else
        nodeVariations balancedHalf1 balancedHalf ++
        nodeVariations balancedHalf balancedHalf1
    where nodeVariations nodes1 nodes2 =
            (\left -> (\right ->
                    (Node value left right)
                ) `map` nodes1
            ) `concatMap` nodes2
          balancedHalf = cBalanced (n `div` 2) value
          balancedHalf1 = cBalanced ((n `div` 2) - 1) value

addLeafNode :: a -> Tree a -> [Tree a]
addLeafNode value End = [leafNode value]
addLeafNode value (Node _ left right) =
    (\it -> Node value it right) `map` leftTrees ++
    (\it -> Node value left it) `map` rightTrees
    where leftTrees = addLeafNode value left
          rightTrees = addLeafNode value right

