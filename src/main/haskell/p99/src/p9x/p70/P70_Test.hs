import Test.HUnit
import P9x.P70.P70
import P9x.Util


p70 = testList "P70" [
    expectEqual 2 (nodeCount (MNode 'a' [(MNode 'b' [])])),
    expectEqual (MNode 'a' []) (stringToMTree "a^"),
    expectEqual (MNode 'a' [(MNode 'b' [(MNode 'c' [])])]) (stringToMTree "abc^^^"),
    expectEqual (MNode 'a' [(MNode 'b' []), (MNode 'c' [])]) (stringToMTree "ab^c^^"),
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

    expectEqual (MNode 'a' []) (parseAsMTree "a^"),
    expectEqual (MNode 'a' [(MNode 'b' [(MNode 'c' [])])]) (parseAsMTree "abc^^^"),
    expectEqual (MNode 'a' [(MNode 'b' []), (MNode 'c' [])]) (parseAsMTree "ab^c^^"),
    expectEqual
        (MNode 'a' [
            (MNode 'f' [MNode 'g' []]),
            (MNode 'c' []),
            (MNode 'b' [
                (MNode 'd' []),
                (MNode 'e' [])
            ])
        ])
        (parseAsMTree "afg^^c^bd^e^^^"),

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

p73 = testList "P73" [
    expectEqual "a" (toLispyTree (stringToMTree "a^")),
    expectEqual "(a b c)" (toLispyTree (stringToMTree "ab^c^^")),
    expectEqual "(a (b c))" (toLispyTree (stringToMTree "abc^^^")),
    expectEqual "(a (f g) c (b d e))" (toLispyTree (stringToMTree "afg^^c^bd^e^^^")),

    expectEqual (stringToMTree "a^") (fromLispyTree "a"),
    expectEqual (stringToMTree "ab^c^^") (fromLispyTree "(a b c)"),
    expectEqual (stringToMTree "abc^^^") (fromLispyTree "(a (b c))"),
    expectEqual (stringToMTree "afg^^c^bd^e^^^") (fromLispyTree "(a (f g) c (b d e))"),

    expectEqual (stringToMTree "a^") (parseAsLispyTree "a"),
    expectEqual (stringToMTree "ab^c^^") (parseAsLispyTree "(a b c)"),
    expectEqual (stringToMTree "abc^^^") (parseAsLispyTree "(a (b c))"),
    expectEqual (stringToMTree "afg^^c^bd^e^^^") (parseAsLispyTree "(a (f g) c (b d e))")
 ]

main :: IO Counts
main = runTestTT $ TestList [p70, p71, p72, p73]
