package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Test
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

fun minimumSwaps(array: Array<Int>): Int = findSwapsToSort(array).count()

private data class Swap(val i1: Int, val i2: Int) {
    override fun toString() = "Swap($i1, $i2)"
}

private fun findSwapsToSort(array: Array<Int>): Sequence<Swap> {
    return if (array.size <= 1) emptySequence()
    else sequence {
        while (true) {
            val i = array.indices.find { array[it] != it + 1 && array[it] + 1 != array[it + 1] } ?: break
            val swap = Swap(i, array.findIndexToSwap(array[i]) ?: error(i))
            array.swap(swap.i1, swap.i2)
            yield(swap)
        }
    }
}

private fun Array<Int>.findIndexToSwap(n: Int): Int? {
    return indices.take(n + 1).drop(1).reversed().find { this[it] < this[it - 1] }
}

private fun <T> Array<T>.swap(i1: Int, i2: Int) {
    val tmp = this[i1]
    this[i1] = this[i2]
    this[i2] = tmp
}


class MinimumSwaps2 {
    @Test fun `no swaps`() {
        arrayOf(1) isSortedWithSwaps emptyList()
        arrayOf(1, 2) isSortedWithSwaps emptyList()
    }

    @Test fun `single swap`() {
        arrayOf(2, 1) isSortedWithSwaps listOf(Swap(0, 1))
        arrayOf(3, 2, 1) isSortedWithSwaps listOf(Swap(0, 2))
    }

    @Test fun `two swaps`() {
        arrayOf(3, 1, 2) isSortedWithSwaps listOf(Swap(0, 1), Swap(1, 2))
    }

    @Test fun `testcase from the task`() {
        """|7
           |7 1 3 2 4 5 6
        """ shouldProduce """
           |5
        """
    }

    @Test fun `testcase 0`() {
        """|4
           |4 3 1 2
        """ shouldProduce """
           |3
        """
    }

    @Test fun `testcase 1`() {
        """|5
           |2 3 4 1 5
        """ shouldProduce """
           |3
        """
    }

    @Test fun `testcase 2`() {
        """|7
           |1 3 5 2 4 6 7
        """ shouldProduce """
           |3
        """
    }

    private infix fun Array<Int>.isSortedWithSwaps(expected: List<Swap>) {
        findSwapsToSort(this).toList() shouldEqual expected
    }

    private infix fun String.shouldProduce(expectedOutput: String) {
        val outputRecorder = OutputRecorder()
        main(toLineSequence(), outputRecorder)
        outputRecorder.text shouldEqual expectedOutput.trimMargin() + "\n"
    }
}