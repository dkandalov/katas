package katas.kotlin.hackerrank

import datsok.shouldEqual
import org.junit.Test
import java.util.*

/**
 * https://www.hackerrank.com/challenges/new-year-chaos
 */
fun main() {
    val scanner = Scanner(System.`in`)
    main(generateSequence { scanner.nextLine() })
}

private fun main(input: Sequence<String>, output: (Any?) -> Unit = { println(it) }) {
    val i = input.iterator()
    val t = i.next().trim().toInt()

    (1..t).forEach {
        i.next().trim().toInt() // skip number of items because it can be inferred from the line itself
        val q = i.next().split(" ").map { token -> token.trim().toInt() }.toTypedArray()
        val bribes = minimumBribes(q)
        output(bribes?.toString() ?: "Too chaotic")
    }
}

fun minimumBribes(q: Array<Int>): Int? {
    if (q.size <= 1) return 0

    var result = 0
    var i = q.size - 2
    while (i >= 0) {
        val bribes = q.moveForward(i)
        if (bribes == null) return null else result += bribes
        i--
    }
    return result
}

fun Array<Int>.moveForward(i: Int): Int? {
    if (this[i] <= this[i + 1]) return 0
    swap(i, i + 1)
    if (i + 2 == size || this[i + 1] <= this[i + 2]) return 1
    swap(i + 1, i + 2)
    if (i + 3 == size || this[i + 2] <= this[i + 3]) return 2
    return null
}

private fun <T> Array<T>.swap(i1: Int, i2: Int) {
    val tmp = this[i1]
    this[i1] = this[i2]
    this[i2] = tmp
}

class NewYearChaosTests {
    @Test fun `testcase 0`() {
        """|2
           |5
           |2 1 5 3 4
           |5
           |2 5 1 3 4
        """ shouldProduce """
           |3
           |Too chaotic
        """
    }

    @Test fun `testcase 1`() {
        """|2
           |8
           |5 1 2 3 7 8 6 4
           |8
           |1 2 5 3 7 8 6 4
        """ shouldProduce """
           |Too chaotic
           |7
        """
    }

    @Test fun `testcase 2`() {
        """|1
           |8
           |1 2 5 3 4 7 8 6
        """ shouldProduce """
           |4
        """
    }

    private infix fun String.shouldProduce(expectedOutput: String) {
        val outputRecorder = OutputRecorder()
        main(trimToLineSequence(), outputRecorder)
        outputRecorder.text shouldEqual expectedOutput.trimMargin() + "\n"
    }
}