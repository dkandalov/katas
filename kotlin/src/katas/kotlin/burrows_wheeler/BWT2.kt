package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlincommon.join
import kotlincommon.printed
import org.junit.Test

class BWT2 {
    @Test fun `encode and decode "banana"`() {
        val encoded = "banana".encode().printed()
        assertThat(encoded, equalTo("|bnn^aaa"))

        val decoded = encoded.decode().printed()
        assertThat(decoded, equalTo("banana"))
    }

    private fun String.decode(): String {
        val table = MutableList(length, { "" })
        repeat(length) {
            forEachIndexed { i, c -> table[i] = c + table[i] }
            table.sort()
        }
        return table.find { it.first() == '^' && it.last() == '|' }
                ?.let { it.substring(1, it.length - 1) } ?: ""
    }

    private fun String.encode(): String = ("^" + this + "|").let { s ->
        s.indices
            .map { s.drop(it) + s.take(it) }
            .sorted()
            .map{ it.last() }
            .join("")
    }
}
