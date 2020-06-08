package katas.kotlin.leetcode.subtree_with_max_average

import datsok.shouldEqual
import org.junit.Test
import java.util.*

class SubtreeWithMaxAverageTests {
    @Test fun example() {
        val tree = NTreeNode(20,
            NTreeNode(12, NTreeNode(11), NTreeNode(2), NTreeNode(3)),
            NTreeNode(18, NTreeNode(15), NTreeNode(8))
        )
//        tree.let {
//            it.size shouldEqual 8
//            it.sum shouldEqual 89
//            it.average.toString() shouldEqual "11.125"
//        }
//        tree.children[0].let {
//            it.size shouldEqual 4
//            it.sum shouldEqual 28
//            it.average.toString() shouldEqual "7.0"
//        }
//        tree.children[1].let {
//            it.size shouldEqual 3
//            it.sum shouldEqual 41
//            it.average.toString() shouldEqual "13.666666666666666"
//        }

        maxAverageOf(tree) shouldEqual 18
    }

    @Test fun `other examples`() {
        maxAverageOf(NTreeNode(123)) shouldEqual null
        maxAverageOf(NTreeNode(1, NTreeNode(2))) shouldEqual 1
        maxAverageOf(NTreeNode(2, NTreeNode(1))) shouldEqual 2
        maxAverageOf(NTreeNode(1, NTreeNode(2), NTreeNode(3))) shouldEqual 1
    }
}

private fun maxAverageOf(nTreeNode: NTreeNode): Int? {
    val sumByNode = HashMap<NTreeNode, Int>()
    val sizeByNode = HashMap<NTreeNode, Int>()
    var maxAverage = Double.MIN_VALUE
    var maxNode: NTreeNode? = null

    nTreeNode.traverseBottomUp { node ->
        sumByNode[node] = node.value + node.children.sumBy { sumByNode[it]!! }
        sizeByNode[node] = 1 + node.children.sumBy { sizeByNode[it]!! }
        val nodeAverage = sumByNode[node]!!.toDouble() / sizeByNode[node]!!
        if (node.children.isNotEmpty() && nodeAverage > maxAverage) {
            maxAverage = nodeAverage
            maxNode = node
        }
    }

    return maxNode?.value
}

private fun NTreeNode.traverseBottomUp(callback: (NTreeNode) -> Unit) {
    this.children.forEach { it.traverseBottomUp(callback) }
    callback(this)
}

private data class NTreeNode(val value: Int, val children: List<NTreeNode> = emptyList()) {
    // Commented out because this is cheating
//    val sum: Int = value + children.sumBy { it.sum }
//    val size: Int = 1 + children.sumBy { it.size }
//    val average: Double = sum.toDouble() / size

    constructor(value: Int, vararg children: NTreeNode): this(value, children.toList())
}