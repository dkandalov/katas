{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module P9x.P50.P50 (
    Tree(..), node, leafNode, t, t_, e,
    sizeOf, isBalanced,
    cBalanced,
    isMirrorOf, isSymmetric,
    addValue, fromList,
    symmetricBalancedTrees,
    heightOf, isHeightBalanced, hbalTrees,
    maxHbalNodes, minHbalNodes, minHbalHeight, maxHbalHeight, hbalTreesWithNodes,
    leafCount, leafList,
    internalList, atLevel,
    completeBinaryTree,
    XY(..), layoutBinaryTree, layoutBinaryTree2, layoutBinaryTree3,
    GShow(..), toString, fromString, parseFromString,
    preorder, inorder, preInTree,
    toDotString, fromDotString
) where

import Data.List
import Data.Maybe(fromJust)
import Text.ParserCombinators.Parsec

data Tree a = Node { value :: a, left :: Tree a, right :: Tree a } | End
              deriving (Eq)
leafNode :: a -> Tree a
leafNode value = Node value End End
node :: a -> Tree a -> Tree a -> Tree a
node value left right = Node value left right

-- shortcut names for tree construction in code
t = Node
t_ value = Node value End End
e = End


instance (Show a) => Show (Tree a) where
    show End = "e"
    show (Node value End End) = "(t_ " ++ (show value) ++ ")"
    show (Node value left right) =
            "(t " ++ (show value) ++ " " ++
            (show left) ++ " " ++ (show right) ++ ")"


data XY a = XY Int Int a deriving (Eq)

instance (Show a) => Show (XY a) where
    show (XY x y value) = "(XY " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show value) ++ ")"

equalXY :: XY a -> XY a -> Bool
equalXY (XY x1 y1 _) (XY x2 y2 _) = x1 == x2 && y1 == y2


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


-- P57
addValue :: Ord a => Tree a -> a -> Tree a
addValue End value = Node value End End
addValue (Node value left right) newValue =
    if (newValue <= value) then Node value (addValue left newValue) right
    else Node value left (addValue right newValue)

fromList :: Ord a => [a] -> Tree a
fromList list = foldl (\tree value -> addValue tree value) End list


-- P58
symmetricBalancedTrees :: Int -> a -> [Tree a]
symmetricBalancedTrees size value = filter isSymmetric (cBalanced size value)

-- P59
hbalTrees :: Int -> a -> [Tree a]
hbalTrees 0 _ = [End]
hbalTrees 1 value = [(Node value End End)]
hbalTrees height value = combinedTrees
    where combinedTrees =
           (combine fullTree fullTree) ++
           (combine smallTree fullTree) ++
           (combine fullTree smallTree)
          combine left right = [(Node value l r) | l <- left, r <- right]
          fullTree = hbalTrees (height - 1) value
          smallTree = hbalTrees (height - 2) value

heightOf :: Tree a -> Int
heightOf End = 0
heightOf (Node _ left right) = 1 + (max (heightOf left) (heightOf right))

isHeightBalanced :: Tree a -> Bool
isHeightBalanced End = True
isHeightBalanced (Node _ left right) = (abs $ heightOf left - heightOf right) <= 1


-- P60
maxHbalNodes :: Int -> Int
maxHbalNodes height = (2 ^ height) - 1

minHbalNodes :: Int -> Int
minHbalNodes 0 = 0
minHbalNodes 1 = 1
minHbalNodes height = 1 + (minHbalNodes (height - 1)) + (minHbalNodes (height - 2))

maxHbalHeight :: Int -> Int
maxHbalHeight nodeAmount = case result of
    Just height -> height - 1
    Nothing -> -1
    where result = find (\i -> (minHbalNodes i) > nodeAmount) [1..]

minHbalHeight :: Int -> Int
minHbalHeight 0 = 0
minHbalHeight nodeAmount =
    let amount = nodeAmount - 1 in
    if (amount `rem` 2 == 0) then 1 + (minHbalHeight (amount `div` 2))
    else 1 + (min (minHbalHeight (amount `div` 2)) (minHbalHeight (1 + (amount `div` 2))))

nodeCount :: Tree a -> Int
nodeCount End = 0
nodeCount (Node _ left right) = 1 + (nodeCount left) + (nodeCount right)

hbalTreesWithNodes :: Int -> a -> [Tree a]
hbalTreesWithNodes nodeAmount value =
    (\it -> (nodeCount it) == nodeAmount) `filter` trees
    where range = [(minHbalHeight nodeAmount)..(maxHbalHeight nodeAmount)]
          trees = (\height -> hbalTrees height value) `concatMap` range


-- P61
leafCount :: Tree a -> Int
leafCount End = 0
leafCount (Node _ End End) = 1
leafCount (Node _ left right) = (leafCount left) + (leafCount right)

-- P61A
leafList :: Tree a -> [a]
leafList End = []
leafList (Node value End End) = [value]
leafList (Node _ left right) = (leafList left) ++ (leafList right)


-- P62
internalList :: Tree a -> [a]
internalList End = []
internalList (Node _ End End) = []
internalList (Node value left right) = [value] ++ (internalList left) ++ (internalList right)

-- P62B
atLevel :: Int -> Tree a -> [a]
atLevel _ End = []
atLevel 0 _ = error "Level must be >= 1"
atLevel 1 (Node value _ _) = [value]
atLevel level (Node _ left right) = (atLevel (level - 1) left) ++ (atLevel (level - 1) right)


-- P63
completeBinaryTree :: Int -> a -> Tree a
completeBinaryTree nodeAmount value = generate 1
    where generate amount =
            if (amount > nodeAmount) then End
            else Node value (generate (amount * 2)) (generate (amount * 2 + 1))

-- P64
layoutBinaryTree :: Tree a -> Tree (XY a)
layoutBinaryTree tree = (layoutRight tree 0 1)
    where
          layoutRight End _ _ = End
          layoutRight (Node value left right) parentShift depth =
            (Node
                (XY rightShift depth value)
                (layoutLeft left rightShift (depth + 1))
                (layoutRight right rightShift (depth + 1))
            )
            where rightShift = 1 + parentShift + (nodeCount left)

          layoutLeft End _ _ = End
          layoutLeft (Node value left right) parentShift depth =
            (Node
                (XY leftShift depth value)
                (layoutLeft left leftShift (depth + 1))
                (layoutRight right leftShift (depth + 1))
            )
            where leftShift = parentShift - (nodeCount right) - 1

-- P65
layoutBinaryTree2 :: Tree a -> Tree (XY a)
layoutBinaryTree2 tree = shiftX shiftToOne layedoutTree
    where
        shiftToOne = 1 - (leftmostX layedoutTree)
        layedoutTree = layoutRight tree 0 1 ((heightOf tree) - 1)
        layoutRight End _ _ _ = End
        layoutRight (Node value left right) parentX depth level = xyNode x y value left right level
            where x = parentX + (2 ^ level)
                  y = depth
        layoutLeft End _ _ _ = End
        layoutLeft (Node value left right) parentX depth level = xyNode x y value left right level
            where x = parentX - (2 ^ level)
                  y = depth
        xyNode x y value left right level =
            (Node
                (XY x y value)
                (layoutLeft left x (y + 1) (level - 1))
                (layoutRight right x (y + 1) (level - 1)))

leftmostX :: Tree (XY a) -> Int
leftmostX End = error "Can't find leftmost leaf of empty tree"
leftmostX (Node (XY x _ _) End _) = x
leftmostX (Node _ left _) = leftmostX left

shiftX :: Int -> Tree (XY a) -> Tree (XY a)
shiftX _ End = End
shiftX shift (Node (XY x y value) left right) =
    (Node (XY (x + shift) y value) (shiftX shift left) (shiftX shift right))


-- P66
layoutBinaryTree3 :: Tree a -> Tree (XY a)
layoutBinaryTree3 tree = shiftX shiftToOne layedoutTree
    where shiftToOne = 1 - (leftmostX layedoutTree)
          layedoutTree = doLayout tree 1 1
          doLayout End _ _ = End
          doLayout (Node value left right) x y =
            adjustChildren node
            where node = (Node (XY x y value)
                    (doLayout left (x - 1) (y + 1))
                    (doLayout right (x + 1) (y + 1)))

          adjustChildren End = End
          adjustChildren node@(Node value left right) =
            if (haveOverlap (toList left) (toList right)) then
                (adjustChildren (Node value (shiftX (-1) left) (shiftX 1 right)))
            else node

haveOverlap :: [XY a] -> [XY a] -> Bool
haveOverlap [] _ = False
haveOverlap leftList rightList =
    case (find (\it -> (equalXY it (head leftList))) rightList) of
        Nothing -> haveOverlap (tail leftList) rightList
        Just _ -> True

toList :: Tree a -> [a]
toList End = []
toList (Node value left right) = value : (toList left) ++ (toList right)


-- P67
-- http://stackoverflow.com/questions/3740621/removing-string-double-quotes-in-haskell
-- http://stackoverflow.com/questions/2125674/what-is-the-effect-of-type-synonyms-on-instances-of-type-classes-what-does-the
-- Use the class below to avoid quotes when displaying chars, strings
class (Show a) => GShow a where
   gShow :: a -> String
   gShow = show
instance GShow Char where
   gShow it = gShow [it]
instance GShow String where
   gShow = id
instance GShow Int
instance GShow ()

toString :: (GShow a) => Tree a -> String
toString End = ""
toString (Node value End End) = (gShow value)
toString (Node value left right) =
    (gShow value) ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")"


fromString :: String -> Tree Char
fromString xs = snd (fromString' xs)

fromString' :: String -> (String, Tree Char)
fromString' xs@(',':_) = (xs, End)
fromString' xs@(')':_) = (xs, End)
fromString' (x:'(':xs) = (rest, Node x left right)
    where (restLeft, left) = fromString' xs
          (restRight, right) = fromString' (tail restLeft) -- skip ','
          rest = tail restRight -- skip ')'
fromString' (x:xs) = (xs, Node x End End)

parseFromString :: String -> Tree Char
parseFromString input = getEither $ parse node "" input
    where node :: Parser (Tree Char)
          node = try internalNode <|> leafNode
          internalNode =
            do name <- nodeName
               char '('
               left <- node <|> emptyNode
               char ','
               right <- node <|> emptyNode
               char ')'
               return $ Node name left right
          emptyNode =
            do lookAhead $ oneOf ",)"
               return $ End
          leafNode =
            do name <- nodeName
               return $ Node name End End
          nodeName = noneOf "(),"

getEither :: (Show left) => (Either left right) -> right
getEither e = case e of
    Left parseError -> error $ show parseError
    Right value -> value


-- P68
preorder :: Tree a -> [a]
preorder End = []
preorder (Node value left right) = value : (preorder left) ++ (preorder right)

inorder :: Tree a -> [a]
inorder End = []
inorder (Node value left right) = (inorder left) ++ [value] ++ (inorder right)

preInTree :: (Eq a) => [a] -> [a] -> Tree a
preInTree [] [] = End
preInTree preOrdered inOrdered = (Node value
    (preInTree leftPreOrdered leftInOrdered)
    (preInTree rightPreOrdered rightInOrdered))
    where
        value = head preOrdered
        leftPreOrdered = take valueIndexInOrdered (tail preOrdered)
        rightPreOrdered = drop valueIndexInOrdered (tail preOrdered)
        leftInOrdered = take valueIndexInOrdered inOrdered
        rightInOrdered = drop (valueIndexInOrdered + 1) inOrdered
        valueIndexInOrdered = (fromJust (elemIndex value inOrdered)) :: Int


-- P69
toDotString :: Tree Char -> String
toDotString End = "."
toDotString (Node value left right) = value : (toDotString left) ++ (toDotString right)

fromDotString :: String -> Tree Char
fromDotString s = fst $ fromDotString' s

fromDotString' :: String -> (Tree Char, String)
fromDotString' ('.':xs) = (End, xs)
fromDotString' (value:xs) = ((Node value left right), rightRest)
    where (left, leftRest) = fromDotString' xs
          (right, rightRest) = fromDotString' leftRest

