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
        (graphFromString `map` ["[a-b, b-c]", "[a-c, b-c]", "[a-b, a-c]"])
        (spanningTrees $ graphFromString "[a-b, b-c, a-c]")
--    expectEqual
--        []
--        (spanningTrees $ graphFromString "[a-b, b-c, a-c, d]")
 ]

main :: IO Counts
main = runTestTT $ TestList [p80, p81, p82, p83]
