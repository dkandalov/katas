package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlincommon.printed
import kotlincommon.join
import org.junit.Test

class BWT3 {
    @Test fun `encode and decode string`() {
        // See https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
        val encoded = "banana".encode()
        assertThat(encoded, equalTo("bnn{aa}a"))
        assertThat(encoded.decode(), equalTo("banana"))
    }

    // Not using line start/end characters "^|" because they don't really sort the same way as in wikipedia example.
    private fun String.encode(): String = ("{" + this + "}").run {
        return indices
                .map { drop(it) + take(it) }.printed{ it.join("\n") + "\n" }
                .sorted().printed{ it.join("\n") + "\n" }
                .map { it.last() }.join("").printed()
    }

    private fun String.decode(): String {
        val table = MutableList(length, { "" })
        repeat(length) {
            indices.forEach { i -> table[i] = this[i] + table[i] }
            table.sort()
            table.printed{ it.join("\n") + "\n" }
        }
        return table.find { it.startsWith("{") && it.endsWith("}") }!!.let {
            it.substring(1, it.length - 1)
        }
    }
}