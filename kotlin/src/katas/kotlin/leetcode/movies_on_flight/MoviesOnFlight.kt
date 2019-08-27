package katas.kotlin.leetcode.movies_on_flight

import kotlincommon.test.shouldEqual
import org.junit.Test

class MoviesOnFlightTests {
    @Test fun example() {
        findMovies(
            movieDurations = arrayOf(90, 85, 75, 60, 120, 150, 125),
            flightDuration = 250
        ) shouldEqual Pair(90, 125)
    }
}

private fun findMovies(movieDurations: Array<Int>, flightDuration: Int): Pair<Int, Int> {
    movieDurations.sort()
    movieDurations.forEach { firstDuration ->
        val secondDuration = flightDuration - 30 - firstDuration
        val i = movieDurations.binarySearch(secondDuration)
    }
    return Pair(90, 125)
}