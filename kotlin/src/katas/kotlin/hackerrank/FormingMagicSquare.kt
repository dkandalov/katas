package katas.kotlin.hackerrank

import kotlincommon.permutations
import kotlincommon.printed
import java.util.*

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val i = generateSequence { scanner.nextLine() }.iterator()
    main({ i.next() })
}

private fun main(readLine: () -> String, writeLine: (Any?) -> Unit = { println(it) }) {
    val square = arrayOf(
        readLine().split(" ").map { it.toInt() }.toTypedArray(),
        readLine().split(" ").map { it.toInt() }.toTypedArray(),
        readLine().split(" ").map { it.toInt() }.toTypedArray()
    )
    writeLine(formingMagicSquare(square))
}

fun formingMagicSquare(square: Array<Array<Int>>): Int {
    val magicSquares = listOf(
        arrayOf(
            arrayOf(2, 7, 6),
            arrayOf(9, 5, 1),
            arrayOf(4, 3, 8)
        ),
        arrayOf(
            arrayOf(2, 9, 4),
            arrayOf(7, 5, 3),
            arrayOf(6, 1, 8)
        ),
        arrayOf(
            arrayOf(4, 3, 8),
            arrayOf(9, 5, 1),
            arrayOf(2, 7, 6)
        ),
        arrayOf(
            arrayOf(4, 9, 2),
            arrayOf(3, 5, 7),
            arrayOf(8, 1, 6)
        ),
        arrayOf(
            arrayOf(6, 1, 8),
            arrayOf(7, 5, 3),
            arrayOf(2, 9, 4)
        ),
        arrayOf(
            arrayOf(6, 7, 2),
            arrayOf(1, 5, 9),
            arrayOf(8, 3, 4)
        ),
        arrayOf(
            arrayOf(8, 1, 6),
            arrayOf(3, 5, 7),
            arrayOf(4, 9, 2)
        ),
        arrayOf(
            arrayOf(8, 3, 4),
            arrayOf(1, 5, 9),
            arrayOf(6, 7, 2)
        )
    )

    return magicSquares
        .map { it.distanceTo(square) }
        .min()!!
}

private fun Array<Array<Int>>.distanceTo(square: Array<Array<Int>>): Int {
    return 0.until(3).sumBy { col ->
        0.until(3).sumBy { row ->
            Math.abs(this[col][row] - square[col][row])
        }
    }
}

private fun printAllMagicSquares() {
    listOf(1, 2, 3, 4, 5, 6, 7, 8, 9)
        .permutations()
        .filter { it.isMagic() }
        .map { it.printed() }
}

private fun List<Int>.isMagic(): Boolean {
    val row1 = this[0] + this[1] + this[2]
    val row2 = this[3] + this[4] + this[5]
    val row3 = this[6] + this[7] + this[8]

    val col1 = this[0] + this[3] + this[6]
    val col2 = this[1] + this[4] + this[7]
    val col3 = this[2] + this[5] + this[8]

    val diagonal1 = this[0] + this[4] + this[8]
    val diagonal2 = this[2] + this[4] + this[6]

    val rowsAreEqual = row1 == row2 && row2 == row3
    val colsAreEqual = col1 == col2 && col2 == col3
    return rowsAreEqual && colsAreEqual &&
        row1 == col1 && row1 == diagonal1 && diagonal1 == diagonal2
}

class FormingMagicSquareTests {
    // TODO
    /*
    val square = arrayOf(
        arrayOf(5, 3, 4),
        arrayOf(1, 5, 8),
        arrayOf(6, 4, 2)
    )
*/
    /*val square = arrayOf(
        arrayOf(4, 9, 2),
        arrayOf(3, 5, 7),
        arrayOf(8, 1, 5)
    )*/
    /*val square = arrayOf(
        arrayOf(4, 8, 2),
        arrayOf(4, 5, 7),
        arrayOf(6, 1, 6)
    )*/
}