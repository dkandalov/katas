module P9x.Util(
    doExpectEqual, expectEqual, expectEqual_, testList
) where
import Test.HUnit

doExpectEqual :: (Eq a, Show a) => String -> a -> a -> IO Counts
doExpectEqual desc expected actual = (runTestTT (expectEqual desc expected actual))

expectEqual_ :: (Eq a, Show a) => a -> a -> Test
expectEqual_ = expectEqual ""

expectEqual :: (Eq a, Show a) => String -> a -> a -> Test
expectEqual desc expected actual = (TestCase (assertEqual desc expected actual))

testList :: String -> [Test] -> Test
testList description tests = TestLabel description (TestList tests)
