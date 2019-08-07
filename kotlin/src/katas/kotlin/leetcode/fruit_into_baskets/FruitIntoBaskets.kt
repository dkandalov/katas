package katas.kotlin.leetcode.fruit_into_baskets

import kotlincommon.test.shouldEqual
import org.junit.Test

class FruitIntoBasketsTests {
    @Test fun `count amount of fruit collected into baskets`() {
        totalFruit(intArrayOf()) shouldEqual 0
        totalFruit(intArrayOf(1)) shouldEqual 1

        totalFruit(intArrayOf(1, 2)) shouldEqual 2
        totalFruit(intArrayOf(1, 2, 3)) shouldEqual 2

        totalFruit(intArrayOf(1, 2, 1)) shouldEqual 3
        totalFruit(intArrayOf(1, 2, 3, 3)) shouldEqual 3
        totalFruit(intArrayOf(1, 2, 1, 3)) shouldEqual 3
    }

    private fun totalFruit(tree: IntArray): Int {
        var maxCount = 0
        var count = 0
        val bucket = HashSet<Int>()
        (0 until tree.size).forEach { i ->
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