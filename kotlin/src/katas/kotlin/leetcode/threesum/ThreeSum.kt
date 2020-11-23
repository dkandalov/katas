package katas.kotlin.leetcode.threesum

import com.google.common.collect.HashMultiset
import com.google.common.collect.Multiset
import datsok.shouldEqual
import org.junit.Test
import kotlin.random.Random
import kotlin.random.nextInt

//
// https://leetcode.com/problems/3sum ✅
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
// why?
// naming :| (hard to search for it online)
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
        listOf<Int>().findZeroSumTriplets() shouldEqual emptySet()
        listOf(0).findZeroSumTriplets() shouldEqual emptySet()

        listOf(-1, 0, 1).findZeroSumTriplets() shouldEqual setOf(Triplet(-1, 0, 1))
        listOf(-1, 0, 1, 1).findZeroSumTriplets() shouldEqual setOf(Triplet(-1, 0, 1))
        listOf(0, 0, 0, 0).findZeroSumTriplets() shouldEqual setOf(Triplet(0, 0, 0))
        listOf(1, 2, 3).findZeroSumTriplets() shouldEqual emptySet()

        listOf(-1, 0, 1, 2, -1, -4).findZeroSumTriplets() shouldEqual setOf(
            Triplet(-1, -1, 2),
            Triplet(-1, 0, 1)
        )
    }

//    @Ignore
    @Test fun `three sum of huge array`() {
        val random = Random(seed = 123)
        val largeList = generateSequence { random.nextInt(range = -100..100) }.take(3000).toList()
        largeList.findZeroSumTriplets().size shouldEqual 5101
    }
}

fun threeSum(nums: IntArray): List<List<Int>> {
    return nums.toList()
        .findZeroSumTriplets()
        .map { it.toList() }
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
                else    -> result.add(Triplet(it[i], it[start++], it[end--]).sorted())
            }
        }
    }
    return result
}

private typealias Triplet = Triple<Int, Int, Int>

private fun Triplet.sorted(): Triplet =
    when {
        second < first -> Triple(second, first, third).sorted()
        third < second -> Triple(first, third, second).sorted()
        else           -> this
    }

private fun Triplet.sum(): Int =
    first + second + third

private fun List<Int>.findZeroSumTriplets_(): Set<Triplet> {
    val result = LinkedHashSet<Triplet>()
    (0..lastIndex - 2).forEach { i ->
        (i + 1..lastIndex - 1).forEach { j ->
            (j + 1..lastIndex).forEach { k ->
                val triplet = Triplet(this[i], this[j], this[k])
                if (triplet.sum() == 0) result.add(triplet.sorted())
            }
        }
    }
    return result
}

/*
class Triplet private constructor(value: Triple<Int, Int, Int>) :
    Value<Triple<Int, Int, Int>>(value, validation = { it.first <= it.second && it.second <= it.third }) {

    val sum = value.first + value.second + value.third

    constructor(val1: Int, val2: Int, val3: Int) : this(Triple(val1, val2, val3).sorted())

    override fun toString() = value.toString()
}
*/

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
