import Test.HUnit
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set


areAnagrams :: String -> String -> Bool
areAnagrams s1 s2 =
    length s1 == length s2 &&
    Set.fromList(s1) == Set.fromList(s2) &&
    any (== s1) (permutations s2)

isSetOfAnagrams :: [String] -> Bool
isSetOfAnagrams xs = all (\(s1, s2) -> areAnagrams s1 s2) (zip xs (tail xs))


tests :: [Test]
tests = [
    TestCase $ assertEqual "" True (areAnagrams "" ""),
    TestCase $ assertEqual "" False (areAnagrams "a" ""),
    TestCase $ assertEqual "" False (areAnagrams "a" "b"),
    TestCase $ assertEqual "" False (areAnagrams "a" "aa"),
    TestCase $ assertEqual "" True (areAnagrams "aa" "aa"),
    TestCase $ assertEqual "" True (areAnagrams "abc" "cba"),
    TestCase $ assertEqual "" True (isSetOfAnagrams ["abc", "cba"]),
    TestCase $ assertEqual "" False (isSetOfAnagrams ["abc", "cba", "abb"])
 ]

main :: IO Counts
main = do runTestTT $ TestList $ map (TestLabel "") tests