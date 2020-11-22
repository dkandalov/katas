package katas.kotlin.leetcode.threesum

import com.google.common.collect.HashMultiset
import com.google.common.collect.Multiset
import datsok.shouldEqual
import nonstdlib.listOfInts
import org.junit.Test
import kotlin.random.Random
import kotlin.random.nextInt

//
// https://leetcode.com/problems/3sum
// https://en.wikipedia.org/wiki/3SUM
//
// Given an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0?
// Find all unique triplets in the array which gives the sum of zero.
// Notice that the solution set must not contain duplicate triplets.
// Constraints:
//    0 <= nums.length <= 3000
//    -105 <= nums[i] <= 105
//
// Example 1:
// Input: nums = [-1,0,1,2,-1,-4]
// Output: [[-1,-1,2],[-1,0,1]]
//
// Example 2:
// Input: nums = []
// Output: []
//
// Example 3:
// Input: nums = [0]
// Output: []
//

//
// naming :| (hard to search for it online)
// why?
// bad function name ->
// IntArray is a bad idea
// Set<Triple<Int>> as return type (although Triple is really a Multiset/Bag of size 3)
// n^3 is just fine
//
// conclusions:
//  - unbelievably bad naming
//  - really specific constraints => devs expect precise requirements
//    => reinforces solution-oriented "tasks" (instead of talking about the actual problem)
//    =>
//  - normalise software development
//
//

class ThreeSumTests {
    @Test fun `find all unique triplets in the array which gives the sum of zero`() {
        listOf(-1, 0, 1).findZeroSumTriplets() shouldEqual setOf(Triplet(-1, 0, 1))
        listOf(1, 2, 3).findZeroSumTriplets() shouldEqual emptySet()
        listOf(-1, 0, 1, 2, -1, -4).findZeroSumTriplets() shouldEqual setOf(
            Triplet(-1, -1, 2),
            Triplet(-1, 0, 1)
        )
        listOf<Int>().findZeroSumTriplets() shouldEqual emptySet()
        listOf(0).findZeroSumTriplets() shouldEqual emptySet()
    }

    //    @Ignore
    @Test fun `three sum of huge array`() {
        Random(seed = 123).listOfInts(size = 4000, valuesRange = -100..100).findZeroSumTriplets().size shouldEqual 5101
    }
}

private fun List<Int>.findZeroSumTriplets(): Set<Triplet> = sorted().let {
    val result = LinkedHashSet<Triplet>()
    (0..lastIndex - 2).forEach { i ->
        var start = i + 1
        var end = lastIndex
        while (start < end) {
            val sum = it[i] + it[start] + it[end]
            when {
                sum < 0 -> start++
                sum > 0 -> end--
                else    -> result.add(Triplet(it[i], it[start++], it[end--]))
            }
        }
    }
    return result
}

private fun List<Int>.findZeroSumTriplets_(): Set<Triplet> {
    val result = LinkedHashSet<Triplet>()
    (0..lastIndex - 2).forEach { i ->
        (i + 1..lastIndex - 1).forEach { j ->
            (j + 1..lastIndex).forEach { k ->
                if (this[i] + this[j] + this[k] == 0) {
                    result.add(Triplet(this[i], this[j], this[k]))
                }
            }
        }
    }
    return result
}

data class Triplet(private val values: List<Int>) {
    constructor(val1: Int, val2: Int, val3: Int) : this(listOf(val1, val2, val3).sorted())

    override fun toString() = values.toString()
}

private fun <T> multisetOf(vararg elements: T): Multiset<T> =
    HashMultiset.create(elements.toList())

fun Random.intArray(
    size: Int = -1,
    sizeRange: IntRange = IntRange.EMPTY,
    valuesRange: IntRange = IntRange(Int.MIN_VALUE, Int.MAX_VALUE),
): IntArray {
    require(size != -1 || !sizeRange.isEmpty()) { "`size` or `sizeRange` must be specified (but not both)" }
    var i = if (size != -1) size else nextInt(sizeRange)
    val result = IntArray(i)
    while (--i >= 0) {
        result[i] = nextInt(valuesRange)
    }
    return result
}
