package katas.kotlin.burrows_wheeler

import katas.kotlin.join
import katas.kotlin.shouldEqual
import org.junit.Test

class BWT8 {
    @Test fun `encode string`() {
        "".encode() shouldEqual "}{"
        "banana".encode() shouldEqual "bnn{aa}a"
    }

    @Test fun `decode string`() {
        TODO()
    }

    private fun String.encode() = "{$this}".toCharArray().allRotations().sorted().lastColumn()

    private fun CharArray.allRotations() = toList().allRotations()

    private fun <T> List<T>.allRotations() = indices.map { shift -> rotate(shift).join("") }

    private fun <T> List<T>.rotate(shift: Int) = drop(shift) + take(shift)

    private fun List<String>.lastColumn() = map { it.last() }.joinToString("")
}
