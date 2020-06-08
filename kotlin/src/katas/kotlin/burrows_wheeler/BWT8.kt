package katas.kotlin.burrows_wheeler

import nonstdlib.join
import datsok.shouldEqual
import org.junit.Test

class BWT8 {
    @Test fun `encode string`() {
        "".encode() shouldEqual "}{"
        "banana".encode() shouldEqual "bnn{aa}a"
    }

    @Test fun `decode string`() {
        "}{".decode() shouldEqual ""
        "bnn{aa}a".decode() shouldEqual "banana"
    }

    private fun String.decode(): String {
        fun MutableList<String>.addColumn(s: String) {
            s.toCharArray().forEachIndexed { row, c ->
                this[row] = c + this[row]
            }
        }

        val table = List(length, { "" }).toMutableList()
        indices.forEach {
            table.addColumn(this)
            table.sort()
        }
        return table.find { it.first() == '{' && it.last() == '}' }!!.let {
            it.substring(1, it.length - 1)
        }
    }

    private fun String.encode() =
        "{$this}".toCharArray()
            .allRotations()
            .sorted()
            .lastColumn()

    private fun CharArray.allRotations() = toList().allRotations()

    private fun <T> List<T>.allRotations() = indices.map { shift -> rotate(shift).join("") }

    private fun <T> List<T>.rotate(shift: Int) = drop(shift) + take(shift)

    private fun List<String>.lastColumn() = map { it.last() }.joinToString("")
}
