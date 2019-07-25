package katas.kotlin.leetcode.threesum

import kotlincommon.test.shouldEqual
import org.junit.Test
import kotlin.random.Random
import kotlin.random.nextInt

/**
 * https://leetcode.com/problems/3sum/
 */
class ThreeSumTests {
    @Test fun `find all unique triplets in the array which gives the sum of zero`() {
        intArrayOf(-1, 0, 1).threeSum() shouldEqual listOf(listOf(-1, 0, 1))
        intArrayOf(1, 2, 3).threeSum() shouldEqual listOf()
        intArrayOf(-1, 0, 1, 2, -1, -4).threeSum() shouldEqual listOf(
            listOf(-1, -1, 2),
            listOf(-1, 0, 1)
        )
    }

    @Test fun `three sum of huge array`() {
        Random(seed = 123).intArray(size = 4000, valuesRange = -100..100).threeSum()
    }
}

private fun IntArray.threeSum(): List<List<Int>> {
    sort()
    val result = ArrayList<List<Int>>()
    (0..size - 3).forEach { i ->
        var j = i + 1
        var k = size - 1
        while (j < k) {
            val sum = this[i] + this[j] + this[k]
            if (sum < 0) j++
            else if (sum > 0) k--
            else {
                j++
                k--
            }
        }
    }
    (0..size - 3).forEach { i ->
        (i + 1..size - 2).forEach { j ->
            val index = binarySearch(-(this[i] + this[j]), j + 1)
            if (index >= 0) result.add(listOf(this[i], this[j], this[index]))
        }
    }
    return result.map { it.sorted() }.distinct()
}

private fun IntArray.threeSum_loop(): List<List<Int>> {
    val result = ArrayList<List<Int>>()
    (0..size - 3).forEach { i ->
        (i + 1..size - 2).forEach { j ->
            (j + 1..size - 1).forEach { k ->
                if (this[i] + this[j] + this[k] == 0) {
                    result.add(listOf(this[i], this[j], this[k]))
                }
            }
        }
    }
    return result.map { it.sorted() }.distinct()
}

fun Random.intArray(
    size: Int = -1,
    sizeRange: IntRange = IntRange.EMPTY,
    valuesRange: IntRange = IntRange(Int.MIN_VALUE, Int.MAX_VALUE)
): IntArray {
    require(size != -1 || !sizeRange.isEmpty()) { "`size` or `sizeRange` must be specified (but not both)" }
    var i = if (size != -1) size else nextInt(sizeRange)
    val result = IntArray(i)
    while (--i >= 0) {
        result[i] = nextInt(valuesRange)
    }
    return result
}
