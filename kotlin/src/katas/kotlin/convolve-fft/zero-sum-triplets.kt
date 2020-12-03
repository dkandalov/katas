package katas.kotlin.`convolve-fft`

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertTimeoutPreemptively
import java.time.Duration
import kotlin.random.Random

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
// Input: [-1,0,1,2,-1,-4]; Output: [[-1,-1,2],[-1,0,1]]
// index:   -4-3-2-1 0 1 2 3 4
// freq:     1,0,0,2,1,1,1,0,0
// index0:   0 1 2 3 4 5 6 7 8 9 10
// index:   -8-7-6-5-4-3-2-1 0 1 2 3 4 5 6 7 8
// convolve: 1,0,0,4,2,2,6,4,5,6,3,2,1,0,0,0,0
//
// Example 2:
// Input: []; Output: []
//
// Example 3:
// Input: [0]; Output: []
//

class Tests {
    @Test fun `example 1`() {
        assertThat(
            listOf(-1, 0, 1, 2, -1, -4).findZeroSumTriplets(),
            equalTo(setOf(Triplet(-1, -1, 2), Triplet(-1, 0, 1)))
        )
    }

    @Test fun `example 2`() {
        assertThat(
            listOf<Int>().findZeroSumTriplets(),
            equalTo(emptySet())
        )
    }
    @Test fun `example 3`() {
        assertThat(
            listOf(0).findZeroSumTriplets(),
            equalTo(emptySet())
        )
    }

    @Test fun `performance on large input`() {
        val random = Random(seed = 42)
        val ints = List(size = 3000) { random.nextInt(-105, 106) }

        val triplets = assertTimeoutPreemptively(Duration.ofSeconds(1)) {
            ints.findZeroSumTriplets()
        }

        assertThat(triplets.size, equalTo(5618))
    }
}

data class Triplet(val value: List<Int>) {
    constructor(first: Int, second: Int, third: Int) :
        this(listOf(first, second, third).sorted())

    init {
        require(value.size == 3 && value == value.sorted())
    }
}

fun List<Int>.findZeroSumTriplets(): Set<Triplet> {
    fun Int.toIndex() = this + 105
    fun Int.toValue() = this - 105

    val freq = MutableList(size = 105 + 1 + 105) { 0 }
    this.forEach { freq[it.toIndex()] += 1 }

    val sumFreq = convolve(freq, freq)
        .mapIndexed { index, n ->
            if (index % 2 == 0 && index / 2 < freq.size) n - freq[index / 2] else n
        }.map { it / 2 }

    val result = HashSet<Triplet>()
    sumFreq.forEachIndexed { index, count ->
        if (count > 0) {
            val third = -(index - (105 * 2))
            val thirdIndex = third.toIndex()
            if (freq.indices.contains(thirdIndex) && freq[thirdIndex] > 0) {
                freq.indices.forEach { firstIndex ->
                    val first = firstIndex.toValue()
                    val second = -third - first
                    val secondIndex = second.toIndex()
                    if (firstIndex in (0..thirdIndex - 1) &&
                        secondIndex in (0..thirdIndex - 1) &&
                        freq[firstIndex] > 0 && freq[secondIndex] > 0
                    ) {
                        result.add(Triplet(first, second, third))
                    }
                }
            }
        }
    }
    return result
}

fun List<Int>.findZeroSumTriplets_(): Set<Triplet> {
    sorted().let {
        val result = HashSet<Triplet>()
        0.rangeTo(it.lastIndex - 2).forEach { i ->
            var left = i + 1
            var right = it.lastIndex
            while (left < right) {
                val sum = it[i] + it[left] + it[right]
                when {
                    sum == 0 -> result.add(Triplet(it[i], it[left++], it[right--]))
                    sum < 0  -> left++
                    else     -> right--
                }
            }
        }
        return result
    }
}
