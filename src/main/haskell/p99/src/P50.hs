module P50 (
    Tree(..),
    cBalanced
) where

data Tree a = Node { value :: a, left :: Tree a, right :: Tree a } | End
              deriving (Show, Eq)

-- P55
cBalanced :: Int -> a -> [Tree a]
cBalanced size value = []