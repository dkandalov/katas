package katas.kotlin.hackerrank

import katas.kotlin.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.collections.ArrayList

/**
 * https://www.hackerrank.com/challenges/matrix-rotation-algo
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val i = generateSequence { scanner.nextLine() }.iterator()
    main({ i.next() })
}

private fun main(readLine: () -> String, writeLine: (Any?) -> Unit = { println(it) }) {
    val (rows, columns, rotations) = readLine().trim().split(" ").map { it.toInt() }

    val matrix = Array(rows, { Array(columns, { 0 }) })
    0.until(rows).forEach { row ->
        matrix[row] = readLine().trim().split(" ").map { it.toInt() }.toTypedArray()
    }

    matrixRotation(matrix, rotations)

    matrix.forEach { row ->
        writeLine(row.joinToString(" "))
    }
}

fun matrixRotation(matrix: Array<Array<Int>>, rotations: Int) {
    val shiftedList = matrix.toClockwiseList().let {
        it.drop(rotations) + it.take(rotations)
    }
    shiftedList.applyTo(matrix)
}

private fun Array<Array<Int>>.toClockwiseList(): List<Int> {
    val fromColumn = 0
    val toColumn = first().size - 1
    val fromRow = 0
    val toRow = size - 1

    val result = ArrayList<Int>()
    IntRange(fromColumn, toColumn - 1).forEach { result.add(this[0][it]) }
    IntRange(fromRow, toRow - 1).forEach { result.add(this[it][toColumn]) }
    IntRange(fromColumn + 1, toColumn).reversed().forEach { result.add(this[toRow][it]) }
    IntRange(fromRow + 1, toRow).reversed().forEach { result.add(this[it][fromColumn]) }
    return result
}

private fun List<Int>.applyTo(matrix: Array<Array<Int>>) {
    TODO()
}


class MatrixLayerRotationTests {
    @Test fun `array to clockwise list`() {
        arrayOf(
            arrayOf(1, 2),
            arrayOf(4, 3)
        ).toClockwiseList() shouldEqual
            listOf(1, 2, 3, 4)

        arrayOf(
            arrayOf(1, 2, 3, 4),
            arrayOf(5, 6, 7, 8),
            arrayOf(9, 10, 11, 12),
            arrayOf(13, 14, 15, 16)
        ).toClockwiseList() shouldEqual
            listOf(1, 2, 3, 4, 8, 12, 16, 15, 14, 13, 9, 5)
    }

    @Test fun `no rotations`() {
        """|2 2 0
           |1 2
           |4 3
        """ shouldOutput """
           |1 2
           |4 3
        """
    }

    @Test fun `single rotation`() {
        """|2 2 1
           |1 2
           |4 3
        """ shouldOutput """
           |2 3
           |1 4
        """
    }

    @Test fun `sample input 1`() {
        """|4 4 2
           |1 2 3 4
           |5 6 7 8
           |9 10 11 12
           |13 14 15 16
        """ shouldOutput """
           |3 4 8 12
           |2 11 10 16
           |1 7 6 15
           |5 9 13 14
        """
    }

    private infix fun String.shouldOutput(expectedOutput: String) {
        val outputRecorder = OutputRecorder()
        main(toReadLineFunction(), outputRecorder)
        outputRecorder.text shouldEqual expectedOutput.trimMargin() + "\n"
    }

    private fun String.toReadLineFunction(): () -> String {
        val i = trim().trimMargin().split("\n").asSequence().iterator()
        return { i.next() }
    }
}