package katas.kotlin.graph

import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test

class Traversal4 {
    @Test fun `depth-first traversal`() {
        "[a]".toGraph().let {
            it.dft("a") shouldEqual "a"
            it.dft("x") shouldEqual ""
        }
        "[a-b]".toGraph().let {
            it.dft("a") shouldEqual "a-b"
            it.dft("b") shouldEqual "b-a"
            it.dft("x") shouldEqual ""
        }

        "[a-b, b-c]".toGraph().let {
            it.dft("a") shouldEqual "a-b-c"
            it.dft("b") shouldEqual "b-a-c"
            it.dft("c") shouldEqual "c-b-a"
            it.dft("x") shouldEqual ""
        }

        // a──b1──c
        // └──b2──┘
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let {
            it.dft("a") shouldEqual ""
        }
    }
}

private fun <Value, Label> Graph<Value, Label>.dft(value: Value, separator: String = "-"): String {
    return nodes[value]?.dft(result = Result(emptyList(), emptySet()))?.list?.join(separator) ?: ""
}

private data class Result<T>(val list: List<T>, val visited: Set<T>)

private fun <Value, Label> Graph.Node<Value, Label>.dft(result: Result<Value>): Result<Value> {
    return if (result.visited.contains(value)) result
    else {
        val result1 = Result(result.list + value, result.visited + value)
        neighbors().fold(result1) { (list, visited), node ->
            if (visited.contains(node.value)) Result(list, visited)
            else node.dft(result1)
        }
    }
}
