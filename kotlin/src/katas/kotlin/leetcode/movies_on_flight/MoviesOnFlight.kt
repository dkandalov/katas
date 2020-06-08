package katas.kotlin.leetcode.movies_on_flight

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/discuss/interview-question/313719/Amazon-or-Online-Assessment-2019-or-Movies-on-Flight
 */
class MoviesOnFlightTests {
    @Test fun examples() {
        findMovies(movieDurations = emptyArray(), flightDuration = 100) shouldEqual null
        findMovies(movieDurations = arrayOf(1), flightDuration = 100) shouldEqual null
        findMovies(movieDurations = arrayOf(1, 2), flightDuration = 20) shouldEqual null
        findMovies(movieDurations = arrayOf(1, 2), flightDuration = 100) shouldEqual Pair(1, 2)
        findMovies(movieDurations = arrayOf(1, 2, 3), flightDuration = 100) shouldEqual Pair(2, 3)
        findMovies(
            movieDurations = arrayOf(90, 85, 75, 60, 120, 150, 125),
            flightDuration = 250
        ) shouldEqual Pair(90, 125)
    }

    @Test fun `if two pairs are equal, prefer the one with the longest movie`() {
        findMovies(movieDurations = arrayOf(40, 50, 30, 60), flightDuration = 120) shouldEqual Pair(30, 60)
    }
}

private fun findMovies(movieDurations: Array<Int>, flightDuration: Int): Pair<Int, Int>? {
    var max = 0
    var maxPair: Pair<Int, Int>? = null
    fun updateMax(target: Int, firstIndex: Int, secondIndex: Int) {
        if (firstIndex == secondIndex) return 
        if (secondIndex < 0 || secondIndex >= movieDurations.size) return
        val first = movieDurations[firstIndex]
        val second = movieDurations[secondIndex]
        if (second > target) return

        val sum = first + second
        if (sum > max) {
            max = sum
            maxPair = Pair(first, second)
        }
    }

    movieDurations.sort()

    for ((index, first) in movieDurations.withIndex()) {
        val target = flightDuration - 30 - first
        if (target > 0) {
            val i = movieDurations.binarySearch(target, fromIndex = index + 1)
            if (i >= 0) updateMax(target, index, i)
            else {
                updateMax(target, index, -i - 2)
                updateMax(target, index, -i - 1)
            }
        }
    }
    return maxPair
}

private fun findMovies_loops(movieDurations: Array<Int>, flightDuration: Int): Pair<Int, Int>? {
    movieDurations.sort()
    val target = flightDuration - 30
    var max = 0
    var maxPair: Pair<Int, Int>? = null
    for ((index, first) in movieDurations.withIndex()) {
        for (second in movieDurations.drop(index + 1)) {
            val sum = first + second
            if (sum > target) continue
            if (sum > max) {
                max = sum
                maxPair = Pair(first, second)
            }
        }
    }
    return maxPair
}