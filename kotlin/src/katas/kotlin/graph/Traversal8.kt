package katas.kotlin.graph

import katas.kotlin.graph.Graph.Node
import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test
import java.util.*

class Traversal8 {
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
            it.dft("a") shouldEqual "a-b1-c-b2"
            it.dft("b1") shouldEqual "b1-a-b2-c"
            it.dft("b2") shouldEqual "b2-a-b1-c"
            it.dft("c") shouldEqual "c-b1-a-b2"
        }
    }

    @Test fun `breadth-first traversal`() {
        "[a]".toGraph().let {
            it.bft("a") shouldEqual "a"
            it.bft("x") shouldEqual ""
        }
        "[a-b]".toGraph().let {
            it.bft("a") shouldEqual "a-b"
            it.bft("b") shouldEqual "b-a"
            it.bft("x") shouldEqual ""
        }
        "[a-b, b-c]".toGraph().let {
            it.bft("a") shouldEqual "a-b-c"
            it.bft("b") shouldEqual "b-a-c"
            it.bft("c") shouldEqual "c-b-a"
            it.bft("x") shouldEqual ""
        }
        // a──b1──c
        // └──b2──┘
        "[a-b1, a-b2, b1-c, b2-c]".toGraph().let {
            it.bft("a") shouldEqual "a-b1-b2-c"
            it.bft("b1") shouldEqual "b1-a-c-b2"
            it.bft("b2") shouldEqual "b2-a-c-b1"
            it.bft("c") shouldEqual "c-b1-b2-a"
        }
    }


    private fun <Value, Label> Graph<Value, Label>.dft(value: Value): String {
        return (this.nodes[value] ?: return "").dft().join("-")
    }

    private fun <Value, Label> Graph<Value, Label>.bft(value: Value): String {
        return (this.nodes[value] ?: return "").bft().join("-")
    }

    data class Frame<Value, Label>(val node: Node<Value, Label>, val path: LinkedHashSet<Node<Value, Label>>)

    private fun <Value, Label> Node<Value, Label>.dft(): LinkedHashSet<Node<Value, Label>> {
        val stack = LinkedList<Frame<Value, Label>>()
        stack.add(Frame(this, LinkedHashSet()))
        while (stack.isNotEmpty()) {
            val frame = stack.removeLast()
            if (!frame.path.contains(frame.node)) {
//                stack.add(Frame())
            }
        }
        return stack.remove().path
    }

    private fun <Value, Label> Node<Value, Label>.bft(): LinkedHashSet<Node<Value, Label>> {
        TODO("not implemented")
    }
}
