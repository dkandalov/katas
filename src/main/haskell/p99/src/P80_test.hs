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
        ((graphFromString "[b-c, f-c, g-h, d, f-b, k-f, h-g]") :: Graph Char ())
 ]

main :: IO Counts
main = runTestTT $ TestList [p80]
