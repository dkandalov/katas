@file:Suppress("DuplicatedCode")

package katas.kotlin.leetcode.twosum

import nonstdlib.printed
import datsok.shouldEqual
import org.junit.Test
import kotlin.random.Random

/**
 * https://leetcode.com/problems/two-sum
 */
class TwoSum {
    @Test fun `find indices of numbers that add up to the target number (with loops)`() {
        arrayOf(2, 7, 11, 15).twoSum_it(target = 9) shouldEqual Pair(0, 1)
        arrayOf(2, 9, 0, 7).twoSum_it(target = 9) shouldEqual Pair(0, 3)
        arrayOf(5, 5).twoSum_it(target = 10) shouldEqual Pair(0, 1)
        arrayOf(10, -15).twoSum_it(target = -5) shouldEqual Pair(0, 1)
        arrayOf(-1, -2).twoSum_it(target = -3) shouldEqual Pair(0, 1)

        randomArray(seed = 123, size = 100).printed().twoSum_it(target = 10) shouldEqual Pair(0, 8)
//        0.until(100).forEach {
//            randomArray(seed = 123, size = 10_000_000, min = -1000, max = 1000).twoSum_it(target = 10)
//        }
    }

    @Test fun `find indices of numbers that add up to the target number (with binary search)`() {
        arrayOf(2, 7, 11, 15).twoSum_bs(target = 9) shouldEqual Pair(0, 1)
        arrayOf(2, 9, 0, 7).twoSum_bs(target = 9) shouldEqual Pair(2, 1)
        arrayOf(5, 5).twoSum_bs(target = 10) shouldEqual Pair(0, 1)
        arrayOf(10, -15).twoSum_bs(target = -5) shouldEqual Pair(1, 0)
        arrayOf(-1, -2).twoSum_bs(target = -3) shouldEqual Pair(1, 0)

        randomArray(seed = 123, size = 100).printed().twoSum_bs(target = 10) shouldEqual Pair(43, 82)
//        0.until(100).forEach {
//            randomArray(seed = 123, size = 10_000_000, min = -1000, max = 1000).twoSum_bs(target = 10)
//        }
    }

    private fun randomArray(seed: Int, size: Int, min: Int = 0, max: Int = 10): Array<Int> {
        val random = Random(seed)
        return Array(size) { random.nextInt(min, max) }
    }

    private fun Array<Int>.twoSum_it(target: Int): Pair<Int, Int> {
        indices.forEach { i1 ->
            (i1 + 1).until(size).forEach { i2 ->
                if (this[i1] + this[i2] == target) return Pair(i1, i2)
            }
        }
        error("no solution")
    }

    private fun Array<Int>.twoSum_bs(target: Int): Pair<Int, Int> {
        val sortedArray = sortedArray()
        sortedArray.indices.forEach { i1 ->
            val item1 = sortedArray[i1]
            val item2 = target - item1
            val i2 = sortedArray.binarySearch(element = item2, fromIndex = i1 + 1)
            if (i2 > i1) return Pair(indexOf(item1), lastIndexOf(item2))
        }
        error("no solution")
    }

    private fun Array<Int>.twoSum_set(target: Int): Pair<Int, Int> {
        val set = indices.toSet()
        indices.forEach { i1 ->
            (i1 + 1).until(size).forEach { i2 ->
                if (this[i1] + this[i2] == target) return Pair(i1, i2)
            }
        }
        error("no solution")
    }
}
