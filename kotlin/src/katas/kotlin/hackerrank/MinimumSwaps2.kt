package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import kotlincommon.permutations
import kotlincommon.printed
import org.junit.Test
import java.io.File
import java.util.*

/**
 * https://www.hackerrank.com/challenges/minimum-swaps-2
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    main(generateSequence { scanner.nextLine() })
}

private fun main(input: Sequence<String>, output: (Any?) -> Unit = { println(it) }) {
    val i = input.iterator()
    i.next().trim().toInt() // skip number of items because it can be inferred from the line itself

    val array = i.next().split(" ").map { it.toInt() }.toTypedArray()
    output(minimumSwaps(array))
}

fun minimumSwaps_(array: Array<Int>): Int {
    var result = 0
    var i = 0
    while (i < array.lastIndex) {
        if (array[i] != i + 1) {
            var j = i
            while (array[j] != i + 1) j++
            array.apply(Swap(i, j))
            result++
        }
        i++
    }
    return result
}

fun minimumSwaps(array: Array<Int>): Int = findSwapsToSort(array).count()

private data class Swap(val i1: Int, val i2: Int) {
    override fun toString() = "Swap($i1, $i2)"
}

private fun findSwapsToSort(array: Array<Int>): Sequence<Swap> =
    if (array.size <= 1) emptySequence()
    else {
        var n = 0
        generateSequence {
            val i = IntRange(n, array.lastIndex).find { array[it] != it + 1 } ?: return@generateSequence null

            var j = i
            while (array[j] != i + 1) j++
            // This 👇 really slows down testcase 9
            // val j = IntRange(i, array.lastIndex).find { array[it] == i + 1 } ?: error(i)

            val swap = Swap(i, j)
            array.apply(swap)

            n = i
            swap
        }
    }

private fun <T> Array<T>.apply(swap: Swap) {
    val tmp = this[swap.i1]
    this[swap.i1] = this[swap.i2]
    this[swap.i2] = tmp
}


class MinimumSwaps2 {
    @Test fun `swaps for single element`() {
        arrayOf(1) isSortedWithSwaps listOf()
    }

    @Test fun `swaps for two elements`() {
        arrayOf(1, 2) isSortedWithSwaps listOf()
        arrayOf(2, 1) isSortedWithSwaps listOf(Swap(0, 1))
    }

    @Test fun `swaps for three elements`() {
        arrayOf(1, 2, 3) isSortedWithSwaps listOf()
        arrayOf(1, 3, 2) isSortedWithSwaps listOf(Swap(1, 2))

        arrayOf(2, 1, 3) isSortedWithSwaps listOf(Swap(0, 1))
        arrayOf(2, 3, 1) isSortedWithSwaps listOf(Swap(0, 2), Swap(1, 2))

        arrayOf(3, 1, 2) isSortedWithSwaps listOf(Swap(0, 1), Swap(1, 2))
        arrayOf(3, 2, 1) isSortedWithSwaps listOf(Swap(0, 2))
    }

    @Test fun `swaps for four elements`() {
        arrayOf(2, 1, 4, 3) isSortedWithSwaps listOf(Swap(0, 1), Swap(2, 3))

        val permutations = arrayOf(1, 2, 3, 4).toList().permutations().map { it.toTypedArray() }
        permutations.forEach {
            findSwapsToSort(it).toList().printed()
        }
    }

    @Test fun `testcase from the task`() {
        arrayOf(7, 1, 3, 2, 4, 5, 6) isSortedWithSwaps listOf(
            Swap(0, 1), Swap(1, 3), Swap(3, 4), Swap(4, 5), Swap(5, 6)
        )
        """|7
           |7 1 3 2 4 5 6
        """ shouldOutput """
           |5
        """
    }

    @Test fun `testcase 0`() {
        arrayOf(4, 3, 1, 2) isSortedWithSteps listOf(
            listOf(1, 3, 4, 2) to Swap(0, 2),
            listOf(1, 2, 4, 3) to Swap(1, 3),
            listOf(1, 2, 3, 4) to Swap(2, 3)
        )
        """|4
           |4 3 1 2
        """ shouldOutput """
           |3
        """
    }

    @Test fun `testcase 1`() {
        arrayOf(2, 3, 4, 1, 5) isSortedWithSteps listOf(
            listOf(1, 3, 4, 2, 5) to Swap(0, 3),
            listOf(1, 2, 4, 3, 5) to Swap(1, 3),
            listOf(1, 2, 3, 4, 5) to Swap(2, 3)
        )
        """|5
           |2 3 4 1 5
        """ shouldOutput """
           |3
        """
    }

    @Test fun `testcase 2`() {
        arrayOf(1, 3, 5, 2, 4, 6, 7) isSortedWithSwaps listOf(
            Swap(1, 3), Swap(2, 3), Swap(3, 4)
        )
        """|7
           |1 3 5 2 4 6 7
        """ shouldOutput """
           |3
        """
    }

    @Test fun `testcase 4`() {
        """|50
           |2 31 1 38 29 5 44 6 12 18 39 9 48 49 13 11 7 27 14 33 50 21 46 23 15 26 8 47 40 3 32 22 34 42 16 41 24 10 4 28 36 30 37 35 20 17 45 43 25 19
        """ shouldOutput """
           |46
        """
    }

    @Test fun `testcase 9`() {
        val input = File("src/katas/kotlin/hackerrank/MinimumSwaps2-input09.txt").readText()
        input shouldOutput """
           |49990
        """
    }

    @Test fun `swaps on large array`() {
        val array = Array(100_000) { it + 1 }
        val i2 = array.size - 1
        val tmp = array[0]
        array[0] = array[i2]
        array[i2] = tmp
        minimumSwaps(array) shouldEqual 1
    }

    private infix fun Array<Int>.isSortedWithSwaps(expected: List<Swap>) {
        findSwapsToSort(this).toList() shouldEqual expected
    }

    private infix fun Array<Int>.isSortedWithSteps(expected: List<Pair<List<Int>, Swap>>) {
        findSwapsToSort(this).map { this.toList() to it }.toList() shouldEqual expected
    }

    private infix fun String.shouldOutput(expectedOutput: String) {
        val outputRecorder = OutputRecorder()
        main(toLineSequence(), outputRecorder)
        outputRecorder.text shouldEqual expectedOutput.trimMargin() + "\n"
    }
}