package katas.kotlin.leetcode.largest_subarray_length_k

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/352459/Google-or-OA-Fall-Internship-2019-or-Largest-Subarray-Length-K
 */
class LargestSubarrayLengthKTests {
    @Test fun examples() {
        largestSubarray(emptyArray(), k = 0) shouldEqual emptyArray()

        largestSubarray(arrayOf(1, 2, 3, 4, 5), k = 5) shouldEqual arrayOf(1, 2, 3, 4, 5)
        largestSubarray(arrayOf(1, 2, 3, 4, 5), k = 4) shouldEqual arrayOf(2, 3, 4, 5)
        largestSubarray(arrayOf(1, 2, 3, 4, 5), k = 3) shouldEqual arrayOf(3, 4, 5)
        largestSubarray(arrayOf(1, 2, 3, 4, 5), k = 2) shouldEqual arrayOf(4, 5)
        largestSubarray(arrayOf(1, 2, 3, 4, 5), k = 1) shouldEqual arrayOf(5)

        largestSubarray(arrayOf(1, 4, 3, 2, 5), k = 4) shouldEqual arrayOf(4, 3, 2, 5)
        largestSubarray(arrayOf(1, 4, 3, 2, 5), k = 3) shouldEqual arrayOf(4, 3, 2)
    }

    @Test fun `array comparison`() {
        arrayOf<Int>().compareTo(arrayOf()) shouldEqual 0
        arrayOf(1).compareTo(arrayOf(1)) shouldEqual 0
        arrayOf(0, 1).compareTo(arrayOf(0, 1)) shouldEqual 0
        arrayOf(0, 1).compareTo(arrayOf(0, 1, 2)) shouldEqual 0

        arrayOf(1).compareTo(arrayOf(2)) shouldEqual -1
        arrayOf(0, 1).compareTo(arrayOf(0, 2)) shouldEqual -1
        arrayOf(0, 1).compareTo(arrayOf(0, 2)) shouldEqual -1
        arrayOf(0, 1, 2).compareTo(arrayOf(0, 2)) shouldEqual -1
        arrayOf(0, 1).compareTo(arrayOf(0, 2, 0)) shouldEqual -1

        arrayOf(2).compareTo(arrayOf(1)) shouldEqual 1
        arrayOf(2).compareTo(arrayOf(1, 2)) shouldEqual 1

        arrayOf(1, 2, 4, 3, 5).compareTo(arrayOf(1, 2, 3, 4, 5)) shouldEqual 1
    }
}

private fun largestSubarray(array: Array<Int>, k: Int): Array<Int> {
    if (k == 0) return emptyArray()
    if (k < 0 || k > array.size) error("")

    val max = Array(k) { Int.MIN_VALUE }
    (array.size - k).downTo(0).forEach { i ->
        if (array[i] > max[0]) {
            array.copyInto(max, startIndex = i, endIndex = i + k)
        } else if (array[i] == max[0]) {
            var j = 1
            while (j < k) {
                if (array[i + j] > max[j]) {
                    array.copyInto(max, startIndex = i + j, endIndex = i + j + k)
                    break
                }
                j++
            }
        }
    }
    return max
}

@Suppress("unused")
private fun largestSubarray_(array: Array<Int>, k: Int): Array<Int> {
    if (k == 0) return emptyArray()
    if (k < 0 || k > array.size) error("")
    return array.toList().windowed(size = k, step = 1)
        .sortedWith(Comparator { left, right -> left.compareTo(right) })
        .last().toTypedArray()
}

private fun Array<Int>.compareTo(other: Array<Int>): Int {
    val pair = zip(other).find { it.first != it.second } ?: return 0
    return pair.first.compareTo(pair.second)
}

private fun List<Int>.compareTo(other: List<Int>): Int {
    val pair = zip(other).find { it.first != it.second } ?: return 0
    return pair.first.compareTo(pair.second)
}
