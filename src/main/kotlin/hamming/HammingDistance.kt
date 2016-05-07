package hamming

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

class HammingDistance {
    @Test fun findLongestSetOfWordsWithHammingDistanceAboveTwo() {
        fun distance(a: Int, b: Int): Int {
            var result = 0
            var c = a.xor(b)
            while (c != 0) {
                if (c.and(1) == 1) {
                    result += 1
                }
                c = c.shr(1)
            }
            return result
        }
        assertThat(distance(0, 0), equalTo(0))
        assertThat(distance(1, 1), equalTo(0))
        assertThat(distance(1, 0), equalTo(1))
        assertThat(distance(2, 0), equalTo(1))
        assertThat(distance(3, 1), equalTo(1))
        assertThat(distance(3, 0), equalTo(2))
        assertThat(distance(7, 0), equalTo(3))

        fun intToBinaryString(n: Int, width: Int): String {
            var s = ""
            var i = n
            0.until(width).forEach {
                if (i.and(1) == 1) s = "1" + s else s = "0" + s
                i = i.shr(1)
            }
            return s
        }
        assertThat(intToBinaryString(0, 6), equalTo("000000"))
        assertThat(intToBinaryString(1, 6), equalTo("000001"))
        assertThat(intToBinaryString(2, 6), equalTo("000010"))
        assertThat(intToBinaryString(3, 6), equalTo("000011"))
        assertThat(intToBinaryString(1, 6), equalTo("000001"))

        fun search(options: List<Int>, solution: List<Int>): List<List<Int>> {
            val newOptions = options.filter { option ->
                solution.all { distance(it, option) > 2 }
            }
            if (newOptions.isEmpty()) return listOf(solution)

            return newOptions.flatMap { option ->
                search(newOptions.minus(option), solution.plus(option))
            }
        }
        val solutions = search(0.until(64).toList(), listOf(0))
        println(solutions.size)
    }
}
