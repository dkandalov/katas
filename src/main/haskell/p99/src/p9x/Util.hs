module P9x.Util(
    expectEqual, testList
) where
import Test.HUnit

expectEqual :: (Eq a, Show a) => a -> a -> Test
expectEqual expected actual = TestCase (assertEqual "" expected actual)

testList :: String -> [Test] -> Test
testList description tests = TestLabel description (TestList tests)
