import Test.HUnit
import P50

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Counts
expectEqual desc expected actual = (runTestTT (TestCase (assertEqual desc expected actual)))

main :: IO Counts
main = do
        let xNode = node 'x'
        let xLeaf = leafNode 'x'

        runTestTT $ TestCase $ assertEqual "P55" 3 (sizeOf $ xNode xLeaf xLeaf)
        runTestTT $ TestCase $ assertEqual "P55" True (isBalanced $ xNode xLeaf xLeaf)
        runTestTT $ TestCase $ assertEqual "P55" False (isBalanced $ xNode (xNode xLeaf End) End)
        runTestTT $ TestCase $ assertEqual "P55" [
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
            (cBalanced 4 'x')
        runTestTT $ TestCase $ assertEqual "P55" True ((\it -> isBalanced it) `all` (cBalanced 5 'x'))

        runTestTT $ TestCase $ assertEqual "P56" True (isSymmetric End)
        runTestTT $ TestCase $ assertEqual "P56" True (isSymmetric xLeaf)
        runTestTT $ TestCase $ assertEqual "P56" True (isSymmetric (xNode (xNode xLeaf End) (xNode End xLeaf)))
        runTestTT $ TestCase $ assertEqual "P56" False (isSymmetric (xNode (xNode xLeaf End) (xNode xLeaf End)))

        runTestTT $ TestCase $ assertEqual "P57" (Node 'x' (Node 'a' End End) End) (addValue xLeaf 'a')
        runTestTT $ TestCase $ assertEqual "P57"
            (Node 3 (Node 2 (Node 1 End End) End) (Node 5 End (Node 7 End End)))
            (fromList [3, 2, 5, 7, 1])
        runTestTT $ TestCase $ assertEqual "P57" True (isSymmetric $ fromList [5, 3, 18, 1, 4, 12, 21])
        runTestTT $ TestCase $ assertEqual "P57" False (isSymmetric $ fromList [3, 2, 5, 7, 4])

