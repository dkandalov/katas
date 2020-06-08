package katas.kotlin.burrows_wheeler

import nonstdlib.join
import datsok.shouldEqual
import org.junit.Test

class BWT6 {
    @Test fun `encode string`() {
        "banana".encode() shouldEqual "bnn{aa}a"
        "apple".encode() shouldEqual "{lppa}e"
    }

    @Test fun `decode string`() {
        "bnn{aa}a".decode() shouldEqual "banana"
        "{lppa}e".decode() shouldEqual "apple"
    }
}

private fun String.decode(): String {
    val table = 0.until(length).map{ "" }.toMutableList()
    indices.forEach {
        indices.forEach { row ->
            table[row] = this[row] + table[row]
        }
        table.sort()
    }
    val s = table.find { it.first() == '{' && it.last() == '}' }
    return s?.drop(1)?.dropLast(1) ?: ""
}

private fun String.encode() = ("{" + this + "}").let {
    it.indices
        .map{ shift -> it.shiftLeftBy(shift) }
        .sorted()
        .map{ it.last() }.join("")
}

private fun String.shiftLeftBy(shift: Int) = drop(shift) + take(shift)
