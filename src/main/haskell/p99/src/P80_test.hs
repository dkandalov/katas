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
        (digraphFromString "[b-c, f-c, g-h, d, f-b, k-f, h-g]"),

    expectEqual
        (Digraph [
            (Edge 'b' 'c' 1), (Edge 'f' 'c' 2), (Edge 'g' 'h' 3),
            (Edge 'd' 'd' 0), (Edge 'f' 'b' 4), (Edge 'k' 'f' 5),
            (Edge 'h' 'g' 6)
        ])
        (digraphFromStringLabel "[b-c/1, f-c/2, g-h/3, d, f-b/4, k-f/5, h-g/6]")
 ]

main :: IO Counts
main = runTestTT $ TestList [p80]
