import Test.HUnit
import P50

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Counts
expectEqual desc expected actual = (runTestTT (TestCase (assertEqual desc expected actual)))

main :: IO Counts
main = do
        runTestTT $ TestCase $ assertEqual "P55" [
                (Node 'x'
                    (Node 'x'
                        (Node 'x' End End)
                        End)
                    (Node 'x' End End)),
                (Node 'x'
                    (Node 'x'
                        End
                        (Node 'x' End End))
                    (Node 'x' End End))
            ]
            (cBalanced 4 'x')

