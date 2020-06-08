package katas.kotlin.leetcode.fruit_into_baskets

import datsok.*
import org.junit.*

/**
 * https://leetcode.com/problems/fruit-into-baskets/
 */
class FruitIntoBasketsTests {
    @Test fun `count amount of fruit collected into baskets`() {
        totalFruit(intArrayOf()) shouldEqual 0
        totalFruit(intArrayOf(1)) shouldEqual 1

        totalFruit(intArrayOf(1, 2)) shouldEqual 2
        totalFruit(intArrayOf(1, 2, 3)) shouldEqual 2

        totalFruit(intArrayOf(1, 2, 1)) shouldEqual 3
        totalFruit(intArrayOf(1, 2, 3, 3)) shouldEqual 3
        totalFruit(intArrayOf(1, 2, 1, 3)) shouldEqual 3

        totalFruit(intArrayOf(0, 1, 2, 2)) shouldEqual 3
        totalFruit(intArrayOf(1, 2, 3, 2, 2)) shouldEqual 4
        totalFruit(intArrayOf(3, 3, 3, 1, 2, 1, 1, 2, 3, 3, 4)) shouldEqual 5
    }

    private fun totalFruit(tree: IntArray): Int {
        var maxCount = 0
        var count = 0
        val bucket = HashSet<Int>()
        tree.indices.forEach { i ->
            bucket.add(tree[i])
            if (bucket.size <= 2) {
                count++
            } else {
                maxCount = maxOf(maxCount, count)
                bucket.clear()
                bucket.add(tree[i - 1])
                bucket.add(tree[i])
                count = 2
            }
        }
        maxCount = maxOf(maxCount, count)
        return maxCount
    }
}