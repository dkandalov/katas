package katas.kotlin.leetcode.min_cost_to_connect_ropes

import datsok.shouldEqual
import org.junit.Test
import java.util.*

class MinCostToConnectRopesTests {
    @Test fun example() {
        findMinCost(arrayOf()) shouldEqual 0
        findMinCost(arrayOf(8, 4, 6, 12)) shouldEqual 58
        findMinCost(arrayOf(20, 4, 8, 2)) shouldEqual 54
        findMinCost(arrayOf(1, 2, 5, 10, 35, 89)) shouldEqual 224
        findMinCost(arrayOf(2, 2, 3, 3)) shouldEqual 20
    }
}

private fun findMinCost(ropes: Array<Int>): Int {
    var cost = 0
    val queue = PriorityQueue<Int>()
    queue.addAll(ropes.toList())
    while (queue.size > 1) {
        val first = queue.remove()
        val second = queue.remove()
        queue.add(first + second)
        cost += first + second
    }
    return cost
}
