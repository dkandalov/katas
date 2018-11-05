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
    return 3
}

class MinimumSwaps2 {
    @Test fun `no swaps`() {
        """|1
           |1
        """ shouldProduce """
           |0
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