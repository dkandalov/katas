import Test.HUnit
import P9x.P50.P50
import P9x.Util(testList, expectEqual_)


xNode = node 'x'
xLeaf = leafNode 'x'

p55 = testList "P55" [
    expectEqual_ 3 (sizeOf $ xNode xLeaf xLeaf),
    expectEqual_ True (isBalanced $ xNode xLeaf xLeaf),
    expectEqual_ False (isBalanced $ xNode (xNode xLeaf End) End),
    expectEqual_ [
            xNode
                (xNode xLeaf End)
                xLeaf,
            xNode
                (xNode End xLeaf)
                xLeaf,
            xNode
                xLeaf
                (xNode xLeaf End),
            xNode
                xLeaf
                (xNode End xLeaf)
        ]
        (cBalanced 4 'x'),
    expectEqual_ True ((\it -> isBalanced it) `all` (cBalanced 5 'x'))
 ]

p56 = testList "P56" [
    expectEqual_ True (isSymmetric End),
    expectEqual_ True (isSymmetric xLeaf),
    expectEqual_ True (isSymmetric (xNode (xNode xLeaf End) (xNode End xLeaf))),
    expectEqual_ False (isSymmetric (xNode (xNode xLeaf End) (xNode xLeaf End)))
 ]

p57 = testList "P57" [
    expectEqual_ (Node 'x' (Node 'a' End End) End) (addValue xLeaf 'a'),
    expectEqual_
        (Node 3 (Node 2 (Node 1 End End) End) (Node 5 End (Node 7 End End)))
        (fromList [3, 2, 5, 7, 1]),
    expectEqual_ True (isSymmetric $ fromList [5, 3, 18, 1, 4, 12, 21]),
    expectEqual_ False (isSymmetric $ fromList [3, 2, 5, 7, 4]),
    expectEqual_ -- tree for P65
        (t 'n'
            (t 'k'
                (t 'c'
                    (t_ 'a')
                    (t 'e' (t_ 'd') (t_ 'g')))
                (t_ 'm'))
            (t 'u'
                (t 'p'
                    e
                    (t_ 'q'))
                e))
        (fromList ['n','k','m','c','a','e','d','g','u','p','q'])
 ]

p58 = testList "P58" [
    expectEqual_
        [(t 'x' (t 'x' (t_ 'x') e) (t 'x' e (t_ 'x'))),
         (t 'x' (t 'x' e (t_ 'x')) (t 'x' (t_ 'x') e))]
        (symmetricBalancedTrees 5 'x')
 ]

p59 = testList "P59" [
    expectEqual_ 3 (heightOf (t 'x' (t 'x' e (t_ 'x')) e)),
    expectEqual_ True (isHeightBalanced (t 'x' (t_ 'x') e)),
    expectEqual_ True (isHeightBalanced (t 'x' (t_ 'x') (t_ 'x') )),
    expectEqual_ False (isHeightBalanced (t 'x' (t 'x' e (t_ 'x')) e)),
    expectEqual_ [
            (t 'x' (t_ 'x') (t_ 'x')),
            (t 'x' e (t_ 'x')),
            (t 'x' (t_ 'x') e)
        ]
        (hbalTrees 2 'x'),
    expectEqual_ 15 (length $ hbalTrees 3 'x'),
    expectEqual_ True (all (\it -> (heightOf it) == 3) (hbalTrees 3 'x')),
    expectEqual_ True (all isHeightBalanced (hbalTrees 3 'x'))
    --expectEqual_ [] (hbalTrees 3 'x')
 ]

p60 = testList "P60" [
    expectEqual_ 7 (maxHbalNodes 3),
    expectEqual_ 4 (minHbalNodes 3),
    expectEqual_ 2 (minHbalHeight 3),
    expectEqual_ 2 (maxHbalHeight 3),
    expectEqual_ [ (t 'x' (t_ 'x') (t_ 'x')) ]
        (hbalTreesWithNodes 3 'x'),
    expectEqual_ [
        (t 'x' (t_ 'x') (t 'x' e (t_ 'x'))),
        (t 'x' (t_ 'x') (t 'x' (t_ 'x') e)),
        (t 'x' (t 'x' e (t_ 'x')) (t_ 'x')),
        (t 'x' (t 'x' (t_ 'x') e) (t_ 'x'))
        ]
        (hbalTreesWithNodes 4 'x'),
    expectEqual_ 17 (length $ hbalTreesWithNodes 7 'x'),
    expectEqual_ 1553 (length $ hbalTreesWithNodes 15 'x')
 ]

p61 = testList "P61" [
    expectEqual_ 0 (leafCount End),
    expectEqual_ 2 (leafCount (t 'x' (t 'x' (t_ 'x') e) (t_ 'x'))),
    expectEqual_ [] (leafList End :: [Char]),
    expectEqual_ ['b', 'd', 'e'] (leafList (t 'a' (t_ 'b') (t 'c' (t_ 'd') (t_ 'e'))))
 ]

p62 = testList "P62" [
    expectEqual_ [] (internalList End :: [Char]),
    expectEqual_ ['a', 'c'] (internalList (t 'a' (t_ 'b') (t 'c' (t_ 'd') (t_ 'e')))),
    expectEqual_ ['b', 'c'] (atLevel 2 (t 'a' (t_ 'b') (t 'c' (t_ 'd') (t_ 'e'))))
 ]

p63 = testList "P63" [
    expectEqual_ End (completeBinaryTree 0 'x'),
    expectEqual_ (t_ 'x') (completeBinaryTree 1 'x'),
    expectEqual_ (t 'x' (t_ 'x') e) (completeBinaryTree 2 'x'),
    expectEqual_ (t 'x' (t_ 'x') (t_ 'x')) (completeBinaryTree 3 'x'),
    expectEqual_ (t 'x' (t 'x' (t_ 'x') e) (t_ 'x')) (completeBinaryTree 4 'x'),
    expectEqual_
        (t 'x'
            (t 'x' (t_ 'x') (t_ 'x'))
            (t 'x' (t_ 'x') e))
        (completeBinaryTree 6 'x')
 ]

p64 = testList "P64" [
    expectEqual_ End ((layoutBinaryTree End) :: Tree (XY Char)),
    expectEqual_
        (t (XY 3 1 'a')
            (t (XY 1 2 'b')
                e
                (t_ (XY 2 3 'c')))
            (t_ (XY 4 2 'd')))
        (layoutBinaryTree
            (t 'a' (t 'b' e (t_ 'c')) (t_ 'd'))),
    expectEqual_
        (t (XY 8 1 'n')
            (t (XY 6 2 'k')
                (t (XY 2 3 'c')
                    (t_ (XY 1 4 'a'))
                    (t (XY 5 4 'h') (t (XY 4 5 'g') (t_ (XY 3 6 'e')) e) e))
                (t_ (XY 7 3 'm')))
            (t (XY 12 2 'u')
                (t (XY 9 3 'p')
                    e
                    (t (XY 11 4 's') (t_ (XY 10 5 'q')) e))
                e))
        (layoutBinaryTree (fromList ['n','k','m','c','a','h','g','e','u','p','s','q']))
 ]

p65 = testList "P65" [
    expectEqual_ End ((layoutBinaryTree2 End) :: Tree (XY Char)),
    expectEqual_
        (t (XY 3 1 'a')
            (t (XY 1 2 'b')
                e
                (t_ (XY 2 3 'c')))
            (t_ (XY 5 2 'd')))
        (layoutBinaryTree2
            (t 'a' (t 'b' e (t_ 'c')) (t_ 'd'))),
    expectEqual_
        (t (XY 15 1 'n')
            (t (XY 7 2 'k')
                (t (XY 3 3 'c')
                    (t_ (XY 1 4 'a'))
                    (t (XY 5 4 'e') (t_ (XY 4 5 'd')) (t_ (XY 6 5 'g'))))
                (t_ (XY 11 3 'm')))
            (t (XY 23 2 'u')
                (t (XY 19 3 'p') e (t_ (XY 21 4 'q')))
                e))
        (layoutBinaryTree2 (fromList ['n','k','m','c','a','e','d','g','u','p','q']))
 ]

p66 = testList "P66" [
        expectEqual_ End ((layoutBinaryTree3 End) :: Tree (XY Char)),
        expectEqual_
            (t (XY 2 1 'a')
                (t (XY 1 2 'b')
                    e
                    (t_ (XY 2 3 'c')))
                (t_ (XY 3 2 'd')))
            (layoutBinaryTree3
                (t 'a' (t 'b' e (t_ 'c')) (t_ 'd'))),
        expectEqual_
            (t (XY 5 1 'n')
                (t (XY 3 2 'k')
                    (t (XY 2 3 'c')
                        (t_ (XY 1 4 'a'))
                        (t (XY 3 4 'e') (t_ (XY 2 5 'd')) (t_ (XY 4 5 'g'))))
                    (t_ (XY 4 3 'm')))
                (t (XY 7 2 'u')
                    (t (XY 6 3 'p') e (t_ (XY 7 4 'q')))
                    e)
            )
            (layoutBinaryTree3
               (t 'n'
                   (t 'k'
                       (t 'c'
                           (t_ 'a')
                           (t 'e' (t_ 'd') (t_ 'g')))
                       (t_ 'm'))
                   (t 'u'
                       (t 'p' e (t_ 'q'))
                       e)
               ))
 ]

p67 =
    let tree = (t 'a'
                   (t 'b'
                      (t_ 'd')
                      (t_ 'e'))
                   (t 'c'
                      e
                      (t 'f'
                        (t_ 'g') e)))
        treeString = "a(b(d,e),c(,f(g,)))" in

    testList "P67" [
        expectEqual_ treeString (toString tree),
        expectEqual_ (t 'a' (t_ 'b') (t_ 'c')) (fromString "a(b,c)"),
        expectEqual_ tree (fromString treeString),
        expectEqual_ (t 'a' (t_ 'b') (t_ 'c')) (parseFromString "a(b,c)"),
        expectEqual_ (t 'a' (t_ 'b') e) (parseFromString "a(b,)"),
        expectEqual_ (t 'a' e (t_ 'c')) (parseFromString "a(,c)"),
        expectEqual_ tree (parseFromString treeString)
 ]

p68 = testList "P68" [
    expectEqual_ "abdecfg" (preorder $ fromString "a(b(d,e),c(,f(g,)))"),
    expectEqual_ "dbeacgf" (inorder $ fromString "a(b(d,e),c(,f(g,)))"),
    expectEqual_ (fromString "a(b(d,e),c(,f(g,)))")
        (preInTree "abdecfg" "dbeacgf")
 ]

p69 = testList "P69" [
    expectEqual_ "abd..e..c.fg..." (toDotString $ fromString "a(b(d,e),c(,f(g,)))"),
    expectEqual_ (fromString "a(b(d,e),c(,f(g,)))") (fromDotString $ "abd..e..c.fg...")
 ]

main :: IO Counts
main = runTestTT $ TestList [
    p55, p56, p57, p58, p59,
    p60, p61, p62, p63, p64, p65, p66, p67, p68, p69
 ]
