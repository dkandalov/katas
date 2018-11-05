package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Ignore
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
    var i = 0
    while (i <= array.size - 2) {
        if (array[i] > array[i + 1]) {
            array.swap(i, i + 1)
            swapCount++
        } else {
            i++
        }
    }
    return swapCount
}

private fun <T> Array<T>.swap(i1: Int, i2: Int) {
    val tmp = this[i1]
    this[i1] = this[i2]
    this[i2] = tmp
}


class MinimumSwaps2 {
    @Test fun `no swaps`() {
        """|1
           |1
        """ shouldProduce """
           |0
        """
        """|2
           |1 2
        """ shouldProduce """
           |0
        """
    }

    @Test fun `single swap`() {
        """|2
           |2 1
        """ shouldProduce """
           |1
        """
    }

    @Ignore
    @Test fun `testcase from the task`() {
        """|7
           |7 1 3 2 4 5 6
        """ shouldProduce """
           |5
        """
    }

    @Ignore
    @Test fun `testcase 0`() {
        """|4
           |4 3 1 2
        """ shouldProduce """
           |3
        """
    }

    @Ignore
    @Test fun `testcase 1`() {
        """|5
           |2 3 4 1 5
        """ shouldProduce """
           |3
        """
    }

    @Ignore
    @Test fun `testcase 2`() {
        """|7
           |1 3 5 2 4 6 8
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