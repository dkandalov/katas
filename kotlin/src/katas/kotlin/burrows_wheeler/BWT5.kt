package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.join
import org.junit.Test

class BWT5 {

    @Test fun `encode string`() {
        assertThat("banana".encode(), equalTo("bnn{aa}a"))
        assertThat("apple".encode(), equalTo("{lppa}e"))
    }

    @Test fun `decode string`() {
        assertThat("bnn{aa}a".decode(), equalTo("banana"))
        assertThat("{lppa}e".decode(), equalTo("apple"))
    }

    private fun String.encode(): String = ("{" + this + "}").let {
        it.indices.map { shift -> it.drop(shift) + it.take(shift) }
            .sorted()
            .map{ it.last() }
            .join("")
    }

    private fun String.decode(): String {
        val table = 0.until(length).fold(MutableList(length, {""})) { table: List<String>, _ ->
            indices.map { i -> this[i] + table[i] }.sorted()
        }
        val row = table.find { it.startsWith("{") && it.endsWith("}") }!!
        return row.substring(1, row.length - 1)
    }
}
