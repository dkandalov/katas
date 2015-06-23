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

