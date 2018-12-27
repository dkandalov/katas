package katas.kotlin.skiena.graph2

import kotlincommon.doesNotContain
import kotlincommon.test.shouldEqual
import org.junit.Test

fun <T> Graph<T>.countComponents(): Int {
    var result = 0
    val visitedVertices = HashSet<T>()
    vertices.forEach { vertex ->
        if (visitedVertices.doesNotContain(vertex)) {
            visitedVertices.addAll(bfs(vertex))
            result++
        }
    }
    return result
}

class BFSApplicationTests {
    @Test fun `count components`() {
        Graph.readInts("1-2").countComponents() shouldEqual 1
        Graph.readInts("1-2,2-3").countComponents() shouldEqual 1
        Graph.readInts("1-2,3-4").countComponents() shouldEqual 2
        Graph.readInts("1-2,2-3,5-6").countComponents() shouldEqual 2
        Graph.readInts("1-2,3-4,5-6").countComponents() shouldEqual 3
    }
}