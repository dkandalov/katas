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

fun minimumSwaps(array: Array<Int>): Int {
    if (array.size <= 1) return 0
    var swapCount = 0
    while (true) {
        val i = array.indices.find { array[it] != it + 1 && array[it] + 1 != array[it + 1]} ?: return swapCount
        array.swap(i, array.findIndexToSwap(array[i]) ?: error(i))
        swapCount++
    }
}

private fun Array<Int>.findIndexToSwap(n: Int): Int? {
    return indices.take(n + 1).drop(1).reversed().find { this[it] < this[it - 1] }
}

private fun <T> Array<T>.swap(i1: Int, i2: Int) {
    println("$i1 $i2")
    val tmp = this[i1]
    this[i1] = this[i2]
    this[i2] = tmp
    println(this.toList())
}


class MinimumSwaps2 {
    @Test fun `no swaps`() {
        minimumSwaps(arrayOf(1)) shouldEqual 0
        minimumSwaps(arrayOf(1, 2)) shouldEqual 0
    }

    @Test fun `single swap`() {
        minimumSwaps(arrayOf(2, 1)) shouldEqual 1
        minimumSwaps(arrayOf(3, 2, 1)) shouldEqual 1
    }

    @Test fun `two swaps`() {
        minimumSwaps(arrayOf(3, 1, 2)) shouldEqual 2
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

    private infix fun String.shouldProduce(expectedOutput: String) {
        val outputRecorder = OutputRecorder()
        main(toLineSequence(), outputRecorder)
        outputRecorder.text shouldEqual expectedOutput.trimMargin() + "\n"
    }
}