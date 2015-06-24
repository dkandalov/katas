module P50 (
    Tree(..), node, leafNode,
    sizeOf, isBalanced,
    cBalanced,
    isMirrorOf, isSymmetric
) where

data Tree a = Node { value :: a, left :: Tree a, right :: Tree a } | End
              deriving (Eq)
leafNode :: a -> Tree a
leafNode value = Node value End End
node :: a -> Tree a -> Tree a -> Tree a
node value left right = Node value left right

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

sizeOf :: Tree a -> Int
sizeOf End = 0
sizeOf (Node _ left right) = 1 + (sizeOf left) + (sizeOf right)

isBalanced :: Tree a -> Bool
isBalanced End = True
isBalanced (Node _ left right) = (abs (sizeOf(left) - sizeOf(right))) <= 1


-- P56
isMirrorOf :: Tree a -> Tree a -> Bool
isMirrorOf End End = True
isMirrorOf End (Node _ _ _) = False
isMirrorOf (Node _ _ _) End = False
isMirrorOf (Node _ left1 right1) (Node _ left2 right2) =
    (left1 `isMirrorOf` right2) && (right1 `isMirrorOf` left2)

isSymmetric :: Tree a -> Bool
isSymmetric End = True
isSymmetric (Node _ left right) = left `isMirrorOf` right




addLeafNode :: a -> Tree a -> [Tree a]
addLeafNode value End = [leafNode value]
addLeafNode value (Node _ left right) =
    (\it -> Node value it right) `map` leftTrees ++
    (\it -> Node value left it) `map` rightTrees
    where leftTrees = addLeafNode value left
          rightTrees = addLeafNode value right

