import Test.HUnit
import P70

expectEqual :: (Eq a, Show a) => a -> a -> Test
expectEqual expected actual = TestCase (assertEqual "" expected actual)

testList :: String -> [Test] -> Test
testList description tests = TestLabel description (TestList tests)


p70 = testList "P70" [
    expectEqual 2 (nodeCount (MNode 'a' [(MNode 'b' [])])),
    expectEqual
        (MNode 'a' [
            (MNode 'f' [MNode 'g' []]),
            (MNode 'c' []),
            (MNode 'b' [
                (MNode 'd' []),
                (MNode 'e' [])
            ])
        ])
        (stringToMTree "afg^^c^bd^e^^^"),

    expectEqual
        "afg^^c^bd^e^^^"
        (toString
            (MNode 'a' [
                (MNode 'f' [MNode 'g' []]),
                (MNode 'c' []),
                (MNode 'b' [
                    (MNode 'd' []),
                    (MNode 'e' [])
                ])
            ])
        )
 ]

p71 = testList "P71" [
    expectEqual 9 (internalPathLength (stringToMTree "afg^^c^bd^e^^^"))
 ]

p72 = testList "P72" [
    expectEqual "gfcdeba" (postorder (stringToMTree "afg^^c^bd^e^^^"))
 ]

main :: IO Counts
main = runTestTT $ TestList [p70, p71, p72]
