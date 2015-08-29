import Test.HUnit
import P70_Test(expectEqual, testList)
import P80

p80 = testList "P80" [
    expectEqual
        (Graph [
            (Edge 'b' 'c' ()), (Edge 'f' 'c' ()), (Edge 'g' 'h' ()),
            (Edge 'd' 'd' ()), (Edge 'f' 'b' ()), (Edge 'k' 'f' ()),
            (Edge 'h' 'g' ())
        ])
        (graphFromString "[b-c, f-c, g-h, d, f-b, k-f, h-g]"),

    expectEqual
        (Graph [
            (Edge 'b' 'c' 1), (Edge 'f' 'c' 2), (Edge 'g' 'h' 3),
            (Edge 'd' 'd' 0), (Edge 'f' 'b' 4), (Edge 'k' 'f' 5),
            (Edge 'h' 'g' 6)
        ])
        (graphFromStringLabel "[b-c/1, f-c/2, g-h/3, d, f-b/4, k-f/5, h-g/6]"),

    expectEqual
        (Digraph [
            (Edge 'b' 'c' ()), (Edge 'f' 'c' ()), (Edge 'g' 'h' ()),
            (Edge 'd' 'd' ()), (Edge 'f' 'b' ()), (Edge 'k' 'f' ()),
            (Edge 'h' 'g' ())
        ])
        (digraphFromString "[b>c, f>c, g>h, d, f>b, k>f, h>g]"),

    expectEqual
        (Digraph [
            (Edge 'b' 'c' 1), (Edge 'f' 'c' 2), (Edge 'g' 'h' 3),
            (Edge 'd' 'd' 0), (Edge 'f' 'b' 4), (Edge 'k' 'f' 5),
            (Edge 'h' 'g' 6)
        ])
        (digraphFromStringLabel "[b>c/1, f>c/2, g>h/3, d, f>b/4, k>f/5, h>g/6]"),

    expectEqual
        ("bcfghdk", [
            (Edge 'b' 'c' ()), (Edge 'f' 'c' ()), (Edge 'g' 'h' ()),
            (Edge 'f' 'b' ()), (Edge 'k' 'f' ()), (Edge 'h' 'g' ())
        ])
        (graphToTermForm $ graphFromString "[b-c, f-c, g-h, d, f-b, k-f, h-g]"),

    expectEqual
        [('b', "cf"), ('c',"bf"), ('f',"cbk"), ('g',"h"), ('h',"g"), ('d',""), ('k',"f")]
        (graphToAdjacentForm $ graphFromString "[b-c, f-c, g-h, d, f-b, k-f, h-g]"),

    expectEqual
        ("bcfghdk", [
            (Edge 'b' 'c' ()), (Edge 'f' 'c' ()), (Edge 'g' 'h' ()),
            (Edge 'f' 'b' ()), (Edge 'k' 'f' ()), (Edge 'h' 'g' ())
        ])
        (digraphToTermForm $ digraphFromString "[b>c, f>c, g>h, d, f>b, k>f, h>g]"),

    expectEqual
        [('b', "cf"), ('c',"bf"), ('f',"cbk"), ('g',"h"), ('h',"g"), ('d',""), ('k',"f")]
        (digraphToAdjacentForm $ digraphFromString "[b>c, f>c, g>h, d, f>b, k>f, h>g]")
 ]

p81 = testList "P81" [
    expectEqual
        ["pq", "pmq"]
        (graphFindPaths 'p' 'q' $ graphFromStringLabel "[p-q/9, m-q/7, k, p-m/5]"),
    expectEqual
        ["pq", "pmq"]
        (digraphFindPaths 'p' 'q' $ digraphFromStringLabel "[p>q/9, m>q/7, k, p>m/5]"),
    expectEqual
        []
        (digraphFindPaths 'p' 'k' $ digraphFromStringLabel "[p>q/9, m>q/7, k, p>m/5]")
 ]

p82 = testList "P82" [
    expectEqual
        ["fcbf", "fbcf"]
        (graphFindCycles 'f' $ graphFromString "[b-c, f-c, g-h, d, f-b, k-f, h-g]"),
    expectEqual
        ["abca"]
        (digraphFindCycles 'a' $ digraphFromString "[a>b, b>c, c>a, d]")
 ]

p83 = testList "P83" [
    expectEqual
        (graphFromString `map` ["[a-a]"])
        (spanningTrees $ graphFromString "[a-a]"),
    expectEqual
        (graphFromString `map` ["[a-b]"])
        (spanningTrees $ graphFromString "[a-b]"),
    expectEqual
        (graphFromString `map` ["[a-b, b-c, b-d]"])
        (spanningTrees $ graphFromString "[a-b, b-c, b-d]"),
    expectEqual
        (graphFromString `map` ["[a-b, b-c]", "[a-b, a-c]", "[a-c, b-c]"])
        (spanningTrees $ graphFromString "[a-b, b-c, a-c]"),
    expectEqual
        (graphFromString `map` [])
        (spanningTrees $ graphFromString "[a-b, d]"),
    expectEqual
        (graphFromString `map` [
            "[a-b, a-d, b-c, b-e, d-f, d-g, e-h]",
            "[a-b, a-d, b-c, b-e, d-f, d-g, g-h]",
            "[a-b, a-d, b-c, b-e, d-f, e-h, f-g]",
            "[a-b, a-d, b-c, b-e, d-f, e-h, g-h]",
            "[a-b, a-d, b-c, b-e, d-f, f-g, g-h]",
            "[a-b, a-d, b-c, b-e, d-g, e-h, f-g]",
            "[a-b, a-d, b-c, b-e, d-g, f-g, g-h]",
            "[a-b, a-d, b-c, b-e, e-h, f-g, g-h]",
            "[a-b, a-d, b-c, c-e, d-f, d-g, e-h]",
            "[a-b, a-d, b-c, c-e, d-f, d-g, g-h]",
            "[a-b, a-d, b-c, c-e, d-f, e-h, f-g]",
            "[a-b, a-d, b-c, c-e, d-f, e-h, g-h]",
            "[a-b, a-d, b-c, c-e, d-f, f-g, g-h]",
            "[a-b, a-d, b-c, c-e, d-g, e-h, f-g]",
            "[a-b, a-d, b-c, c-e, d-g, f-g, g-h]",
            "[a-b, a-d, b-c, c-e, e-h, f-g, g-h]",
            "[a-b, a-d, b-c, d-e, d-f, d-g, e-h]",
            "[a-b, a-d, b-c, d-e, d-f, d-g, g-h]",
            "[a-b, a-d, b-c, d-e, d-f, e-h, f-g]",
            "[a-b, a-d, b-c, d-e, d-f, e-h, g-h]",
            "[a-b, a-d, b-c, d-e, d-f, f-g, g-h]",
            "[a-b, a-d, b-c, d-e, d-g, e-h, f-g]",
            "[a-b, a-d, b-c, d-e, d-g, f-g, g-h]",
            "[a-b, a-d, b-c, d-e, e-h, f-g, g-h]",
            "[a-b, a-d, b-c, d-f, d-g, e-h, g-h]",
            "[a-b, a-d, b-c, d-f, e-h, f-g, g-h]",
            "[a-b, a-d, b-c, d-g, e-h, f-g, g-h]",
            "[a-b, a-d, b-e, c-e, d-f, d-g, e-h]",
            "[a-b, a-d, b-e, c-e, d-f, d-g, g-h]",
            "[a-b, a-d, b-e, c-e, d-f, e-h, f-g]",
            "[a-b, a-d, b-e, c-e, d-f, e-h, g-h]",
            "[a-b, a-d, b-e, c-e, d-f, f-g, g-h]",
            "[a-b, a-d, b-e, c-e, d-g, e-h, f-g]",
            "[a-b, a-d, b-e, c-e, d-g, f-g, g-h]",
            "[a-b, a-d, b-e, c-e, e-h, f-g, g-h]",
            "[a-b, a-d, c-e, d-e, d-f, d-g, e-h]",
            "[a-b, a-d, c-e, d-e, d-f, d-g, g-h]",
            "[a-b, a-d, c-e, d-e, d-f, e-h, f-g]",
            "[a-b, a-d, c-e, d-e, d-f, e-h, g-h]",
            "[a-b, a-d, c-e, d-e, d-f, f-g, g-h]",
            "[a-b, a-d, c-e, d-e, d-g, e-h, f-g]",
            "[a-b, a-d, c-e, d-e, d-g, f-g, g-h]",
            "[a-b, a-d, c-e, d-e, e-h, f-g, g-h]",
            "[a-b, a-d, c-e, d-f, d-g, e-h, g-h]",
            "[a-b, a-d, c-e, d-f, e-h, f-g, g-h]",
            "[a-b, a-d, c-e, d-g, e-h, f-g, g-h]",
            "[a-b, b-c, b-e, d-e, d-f, d-g, e-h]",
            "[a-b, b-c, b-e, d-e, d-f, d-g, g-h]",
            "[a-b, b-c, b-e, d-e, d-f, e-h, f-g]",
            "[a-b, b-c, b-e, d-e, d-f, e-h, g-h]",
            "[a-b, b-c, b-e, d-e, d-f, f-g, g-h]",
            "[a-b, b-c, b-e, d-e, d-g, e-h, f-g]",
            "[a-b, b-c, b-e, d-e, d-g, f-g, g-h]",
            "[a-b, b-c, b-e, d-e, e-h, f-g, g-h]",
            "[a-b, b-c, b-e, d-f, d-g, e-h, g-h]",
            "[a-b, b-c, b-e, d-g, e-h, f-g, g-h]",
            "[a-b, b-c, b-e, d-f, e-h, f-g, g-h]",
            "[a-b, b-c, c-e, d-e, d-f, d-g, e-h]",
            "[a-b, b-c, c-e, d-e, d-f, d-g, g-h]",
            "[a-b, b-c, c-e, d-e, d-f, e-h, f-g]",
            "[a-b, b-c, c-e, d-e, d-f, e-h, g-h]",
            "[a-b, b-c, c-e, d-e, d-f, f-g, g-h]",
            "[a-b, b-c, c-e, d-e, d-g, e-h, f-g]",
            "[a-b, b-c, c-e, d-e, d-g, f-g, g-h]",
            "[a-b, b-c, c-e, d-e, e-h, f-g, g-h]",
            "[a-b, b-c, c-e, d-f, d-g, e-h, g-h]",
            "[a-b, b-c, c-e, d-g, e-h, f-g, g-h]",
            "[a-b, b-c, c-e, d-f, e-h, f-g, g-h]",
            "[a-b, b-e, c-e, d-e, d-f, d-g, e-h]",
            "[a-b, b-e, c-e, d-e, d-f, d-g, g-h]",
            "[a-b, b-e, c-e, d-e, d-f, e-h, f-g]",
            "[a-b, b-e, c-e, d-e, d-f, e-h, g-h]",
            "[a-b, b-e, c-e, d-e, d-f, f-g, g-h]",
            "[a-b, b-e, c-e, d-e, d-g, e-h, f-g]",
            "[a-b, b-e, c-e, d-e, d-g, f-g, g-h]",
            "[a-b, b-e, c-e, d-e, e-h, f-g, g-h]",
            "[a-b, b-e, c-e, d-f, d-g, e-h, g-h]",
            "[a-b, b-e, c-e, d-g, e-h, f-g, g-h]",
            "[a-b, b-e, c-e, d-f, e-h, f-g, g-h]",
            "[a-d, b-c, b-e, d-e, d-f, d-g, e-h]",
            "[a-d, b-c, b-e, d-e, d-f, d-g, g-h]",
            "[a-d, b-c, b-e, d-e, d-f, e-h, f-g]",
            "[a-d, b-c, b-e, d-e, d-f, e-h, g-h]",
            "[a-d, b-c, b-e, d-e, d-f, f-g, g-h]",
            "[a-d, b-c, b-e, d-e, d-g, e-h, f-g]",
            "[a-d, b-c, b-e, d-e, d-g, f-g, g-h]",
            "[a-d, b-c, b-e, d-e, e-h, f-g, g-h]",
            "[a-d, b-e, c-e, d-e, d-f, d-g, e-h]",
            "[a-d, b-e, c-e, d-e, d-f, d-g, g-h]",
            "[a-d, b-e, c-e, d-e, d-f, e-h, f-g]",
            "[a-d, b-e, c-e, d-e, d-f, e-h, g-h]",
            "[a-d, b-e, c-e, d-e, d-f, f-g, g-h]",
            "[a-d, b-e, c-e, d-e, d-g, e-h, f-g]",
            "[a-d, b-e, c-e, d-e, d-g, f-g, g-h]",
            "[a-d, b-e, c-e, d-e, e-h, f-g, g-h]",
            "[a-d, b-c, c-e, d-e, d-f, d-g, e-h]",
            "[a-d, b-c, c-e, d-e, d-f, d-g, g-h]",
            "[a-d, b-c, c-e, d-e, d-f, e-h, f-g]",
            "[a-d, b-c, c-e, d-e, d-f, e-h, g-h]",
            "[a-d, b-c, c-e, d-e, d-f, f-g, g-h]",
            "[a-d, b-c, c-e, d-e, d-g, e-h, f-g]",
            "[a-d, b-c, c-e, d-e, d-g, f-g, g-h]",
            "[a-d, b-c, c-e, d-e, e-h, f-g, g-h]",
            "[a-d, b-c, b-e, d-f, d-g, e-h, g-h]",
            "[a-d, b-e, c-e, d-f, d-g, e-h, g-h]",
            "[a-d, b-c, c-e, d-f, d-g, e-h, g-h]",
            "[a-d, b-c, b-e, d-f, e-h, f-g, g-h]",
            "[a-d, b-e, c-e, d-f, e-h, f-g, g-h]",
            "[a-d, b-c, c-e, d-f, e-h, f-g, g-h]",
            "[a-d, b-c, b-e, d-g, e-h, f-g, g-h]",
            "[a-d, b-e, c-e, d-g, e-h, f-g, g-h]",
            "[a-d, b-c, c-e, d-g, e-h, f-g, g-h]"
        ])
        (spanningTrees $ graphFromString
                "[a-b, a-d, b-c, b-e, c-e, d-e, d-f, d-g, e-h, f-g, g-h]")
 ]

p84 = testList "P84" [
    expectEqual
        (graphFromString "[a-a]")
        (minimalSpanningTree $ graphFromString "[a-a]")
 ]

main :: IO Counts
main = runTestTT $ TestList [p80, p81, p82, p83, p84]
