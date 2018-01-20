package katas.kotlin.burrows_wheeler

import katas.kotlin.shouldEqual
import kotlincommon.join
import org.junit.Test

class BWT7 {
    @Test fun `encode string`() {
        "banana".encode() shouldEqual "bnn{aa}a"
        "apple".encode() shouldEqual "{lppa}e"
    }

    @Test fun `decode string`() {
        "bnn{aa}a".decode() shouldEqual "banana"
    }

    private fun String.decode(): String {
        fun Array<String>.insertAsFirstColumn(s: String) {
            s.indices.forEach { i ->
                this[i] = s[i] + this[i]
            }
        }

        val table = Array(length){ "" }
        indices.forEach {
            table.insertAsFirstColumn(this)
            table.sort()
        }
        return table.find { it.first() == '{' && it.last() == '}' }!!.drop(1).dropLast(1)
    }

    private fun String.encode(): String {
        return "{$this}".run {
            indices
                .map { drop(it) + take(it) }.sorted()
                .map { it.last() }.join("")
        }
    }
}
