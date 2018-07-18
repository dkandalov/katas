import graph.BFSTest
import graph.ConnectedComponentsTest
import graph.FindingPathsTest
import graph.GraphTest

fun main(args: Array<String>) {
    // This function exists to make CLion compilation happy and
    // because this currently seems the only to run tests from CLion.

    GraphTest().`create graph from string`()
    BFSTest().`breadth-first search`()
    FindingPathsTest().`find path`()
    ConnectedComponentsTest().`find connected components in a graph`()
}
