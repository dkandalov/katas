package katas.kotlin.hackerrank

import kotlincommon.test.shouldEqual
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
    matrix.listClockwiseSwaps().forEach { swaps ->
        0.until(rotations.rem(swaps.size + 1)).forEach {
            swaps.forEach { swap -> matrix.apply(swap) }
        }
    }
}

private fun Array<Array<Int>>.listClockwiseSwaps(
    fromColumn: Int = 0,
    toColumn: Int = first().size - 1,
    fromRow: Int = 0,
    toRow: Int = size - 1
): List<List<Swap>> {
    val points = ArrayList<Point>()
    IntRange(fromColumn, toColumn - 1).forEach { points.add(Point(fromRow, it)) }
    IntRange(fromRow, toRow - 1).forEach { points.add(Point(it, toColumn)) }
    IntRange(fromColumn + 1, toColumn).reversed().forEach { points.add(Point(toRow, it)) }
    IntRange(fromRow + 1, toRow).reversed().forEach { points.add(Point(it, fromColumn)) }
    val swaps = points.windowed(2).map { Swap(it[0], it[1]) }

    return if (toColumn - fromColumn > 1 && toRow - fromRow > 1) {
        listOf(swaps) + listClockwiseSwaps(fromColumn + 1, toColumn - 1, fromRow + 1, toRow - 1)
    } else {
        listOf(swaps)
    }
}

private fun Array<Array<Int>>.toPrintableString() =
    this.indices.joinToString("\n") { row ->
        this[row].indices.joinToString(" ") { column ->
            this[row][column].toString()
        }
    }

private operator fun Array<Array<Int>>.get(point: Point) = this[point.row][point.column]
private operator fun Array<Array<Int>>.set(point: Point, value: Int) {
    this[point.row][point.column] = value
}

private fun Array<Array<Int>>.apply(swap: Swap) {
    val tmp = this[swap.p1]
    this[swap.p1] = this[swap.p2]
    this[swap.p2] = tmp
}

private data class Point(val row: Int, val column: Int) {
    override fun toString() = "[$row,$column]"
}

private data class Swap(val p1: Point, val p2: Point) {
    override fun toString() = "Swap($p1,$p2)"
}


class MatrixLayerRotationTests {
    @Test fun `2x2 array clockwise swaps`() {
        val array = arrayOf(
            arrayOf(1, 2),
            arrayOf(4, 3)
        )
        array.listClockwiseSwaps() shouldEqual listOf(
            listOf(
                Swap(Point(0, 0), Point(0, 1)),
                Swap(Point(0, 1), Point(1, 1)),
                Swap(Point(1, 1), Point(1, 0))
            )
        )

        matrixRotation(array, 1)
        array.toPrintableString() shouldEqual """
                |2 3
                |1 4
            """.trimMargin()
    }

    @Test fun `2x2 array full cycle rotation`() {
        val array = arrayOf(
            arrayOf(1, 2),
            arrayOf(4, 3)
        )
        matrixRotation(array, 5)
        array.toPrintableString() shouldEqual """
                |2 3
                |1 4
            """.trimMargin()
    }

    @Test fun `4x4 array clockwise swaps`() {
        val array = arrayOf(
            arrayOf(1, 2, 3, 4),
            arrayOf(5, 6, 7, 8),
            arrayOf(9, 10, 11, 12),
            arrayOf(13, 14, 15, 16)
        )
        array.listClockwiseSwaps() shouldEqual listOf(
            // swaps for outer layer
            listOf(
                Swap(Point(0, 0), Point(0, 1)), Swap(Point(0, 1), Point(0, 2)), Swap(Point(0, 2), Point(0, 3)),
                Swap(Point(0, 3), Point(1, 3)), Swap(Point(1, 3), Point(2, 3)), Swap(Point(2, 3), Point(3, 3)),
                Swap(Point(3, 3), Point(3, 2)), Swap(Point(3, 2), Point(3, 1)), Swap(Point(3, 1), Point(3, 0)),
                Swap(Point(3, 0), Point(2, 0)), Swap(Point(2, 0), Point(1, 0))
            ),
            // swaps for inner layer
            listOf(
                Swap(Point(1, 1), Point(1, 2)), Swap(Point(1, 2), Point(2, 2)), Swap(Point(2, 2), Point(2, 1))
            )
        )

        matrixRotation(array, 1)
        array.toPrintableString() shouldEqual """
                |2 3 4 8
                |1 7 11 12
                |5 6 10 16
                |9 13 14 15
            """.trimMargin()
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

    @Test fun `sample input 2`() {
        """|5 4 7
           |1 2 3 4
           |7 8 9 10
           |13 14 15 16
           |19 20 21 22
           |25 26 27 28
        """ shouldOutput """
           |28 27 26 25
           |22 9 15 19
           |16 8 21 13
           |10 14 20 7
           |4 3 2 1
        """
    }

    @Test fun `sample input 3`() {
        """|2 2 3
           |1 1
           |1 1
        """ shouldOutput """
           |1 1
           |1 1
        """
    }

    private infix fun String.shouldOutput(expectedOutput: String) {
        val outputRecorder = OutputRecorder()
        main(toReadLineFunction(), outputRecorder)
        outputRecorder.text shouldEqual expectedOutput.trimMargin() + "\n"
    }
}
