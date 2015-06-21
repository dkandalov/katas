import Test.HUnit
import P50

expectEqual :: (Eq a, Show a) => String -> a -> a -> IO Counts
expectEqual desc expected actual = (runTestTT (TestCase (assertEqual desc expected actual)))

main :: IO Counts
main = do
        runTestTT $ TestCase $ assertEqual "P55"
            []
            (cBalanced 4 'x')

