import Test.HUnit
import P9x.P70.P70
import P9x.Util


p70 = testList "P70" [
    expectEqual_ 2 (nodeCount (MNode 'a' [(MNode 'b' [])])),
    expectEqual_ (MNode 'a' []) (stringToMTree "a^"),
    expectEqual_ (MNode 'a' [(MNode 'b' [(MNode 'c' [])])]) (stringToMTree "abc^^^"),
    expectEqual_ (MNode 'a' [(MNode 'b' []), (MNode 'c' [])]) (stringToMTree "ab^c^^"),
    expectEqual_
        (MNode 'a' [
            (MNode 'f' [MNode 'g' []]),
            (MNode 'c' []),
            (MNode 'b' [
                (MNode 'd' []),
                (MNode 'e' [])
            ])
        ])
        (stringToMTree "afg^^c^bd^e^^^"),

    expectEqual_ (MNode 'a' []) (parseAsMTree "a^"),
    expectEqual_ (MNode 'a' [(MNode 'b' [(MNode 'c' [])])]) (parseAsMTree "abc^^^"),
    expectEqual_ (MNode 'a' [(MNode 'b' []), (MNode 'c' [])]) (parseAsMTree "ab^c^^"),
    expectEqual_
        (MNode 'a' [
            (MNode 'f' [MNode 'g' []]),
            (MNode 'c' []),
            (MNode 'b' [
                (MNode 'd' []),
                (MNode 'e' [])
            ])
        ])
        (parseAsMTree "afg^^c^bd^e^^^"),

    expectEqual_
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
    expectEqual_ 9 (internalPathLength (stringToMTree "afg^^c^bd^e^^^"))
 ]

p72 = testList "P72" [
    expectEqual_ "gfcdeba" (postorder (stringToMTree "afg^^c^bd^e^^^"))
 ]

p73 = testList "P73" [
    expectEqual_ "a" (toLispyTree (stringToMTree "a^")),
    expectEqual_ "(a b c)" (toLispyTree (stringToMTree "ab^c^^")),
    expectEqual_ "(a (b c))" (toLispyTree (stringToMTree "abc^^^")),
    expectEqual_ "(a (f g) c (b d e))" (toLispyTree (stringToMTree "afg^^c^bd^e^^^")),

    expectEqual_ (stringToMTree "a^") (fromLispyTree "a"),
    expectEqual_ (stringToMTree "ab^c^^") (fromLispyTree "(a b c)"),
    expectEqual_ (stringToMTree "abc^^^") (fromLispyTree "(a (b c))"),
    expectEqual_ (stringToMTree "afg^^c^bd^e^^^") (fromLispyTree "(a (f g) c (b d e))"),

    expectEqual_ (stringToMTree "a^") (parseAsLispyTree "a"),
    expectEqual_ (stringToMTree "ab^c^^") (parseAsLispyTree "(a b c)"),
    expectEqual_ (stringToMTree "abc^^^") (parseAsLispyTree "(a (b c))"),
    expectEqual_ (stringToMTree "afg^^c^bd^e^^^") (parseAsLispyTree "(a (f g) c (b d e))")
 ]

main :: IO Counts
main = runTestTT $ TestList [p70, p71, p72, p73]
