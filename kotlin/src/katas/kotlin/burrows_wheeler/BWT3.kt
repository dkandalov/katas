package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.*
import org.junit.Test

class BWT3 {
    @Test fun `encode and decode string`() {
        val encoded = "banana".encode()
        assertThat(encoded, equalTo("|bnn^aaa"))
        assertThat(encoded.decode(), equalTo("banana"))
    }

    private fun String.encode(): String = ("^" + this + "|").run {
        return indices
                .map { drop(it) + take(it) }.printed()
                .sorted().printed()
                .map { it.last() }.join().printed()
    }

    private fun String.decode(): String {
        val table = MutableList(length, { "" })
        repeat(length) {
            indices.forEach { i -> table[i] = this[i] + table[i] }
            table.sort()
            table.printed()
        }
        return table.find { it.startsWith("^") && it.endsWith("|") }!!.let {
            it.substring(1, it.length - 1)
        }
    }
}