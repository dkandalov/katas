import graph.*

fun main(args: Array<String>) {
    // This function exists to make CLion compilation happy and
    // because this currently seems the only to run tests from CLion.

    GraphTest().`create graph from string`()
    BfsTest().`breadth-first graph traversal`()
    FindingPathsTest().`find path`()
    ConnectedComponentsTest().`find connected components in a graph`()
    TwoColourTest().`assign two colours to graph vertexes`()
    DfsTest().`depth-first graph traversal`()
    FindCyclesTest().`find cycle in a graph`()
}
