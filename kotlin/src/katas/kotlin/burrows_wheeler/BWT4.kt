package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlincommon.join
import org.junit.Test

class BWT4 {
    @Test fun `encode and decode`() {
        val encoded = "banana".encode()
        assertThat(encoded, equalTo("bnn{aa}a"))
        assertThat(encoded.decode(), equalTo("banana"))
    }

    private fun String.encode(): String = ("{" + this + "}").run {
        indices
            .map { shift -> drop(shift) + take(shift) }
            .sorted()
            .map{ it.last() }
            .join("")
    }

    private fun String.decode(): String {
        val table = MutableList(length, { "" })
        repeat(times = length) {
            indices.forEach{ i ->
                table[i] = this[i] + table[i]
            }
            table.sort()
        }
        return table.find{ it.startsWith("{") && it.endsWith("}")}!!.let {
            it.substring(1, it.length - 1)
        }
    }
}
