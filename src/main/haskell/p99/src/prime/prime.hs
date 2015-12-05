import Test.HUnit
import Data.List.Ordered(minus)

primesTo :: Integer -> [Integer]
primesTo m = eratos [2..m]
    where eratos [] = []
          eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])
          eratos' (p:xs) = p : eratos (xs `minus` map (p*) [1..])
          eulers (p:xs) = p : eulers (xs `minus` map (p*) (p:xs))


tests :: [Test]
tests = [
    TestCase $ assertEqual "" [2,3,5,7,11,13,17,19] (primesTo 20)
 ]

main :: IO Counts
main = do runTestTT $ TestList $ map (TestLabel "") tests