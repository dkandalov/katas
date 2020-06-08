package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import nonstdlib.join
import nonstdlib.printed
import org.junit.Test

class BWT1 {
    @Test fun `encode and decode a string`() {
        val s = "banana".wrap('^', '|')

        val encoded = s.indices
                .map{ s.rotate(it) }
                .sorted()
                .map{ it.last() }
                .join("")
        assertThat(encoded.printed(), equalTo("|bnn^aaa"))

        val table = MutableList(encoded.length, { ArrayList<Char>() })
        repeat(times = encoded.length) {
            encoded.indices.forEach { i -> table[i].add(0, encoded[i]) }
            table.sortBy{ it[0] }
        }
        val decoded = table
                .map{ it.join("").unwrap('^', '|') }
                .find { it != null }!!
        assertThat(decoded.printed(), equalTo("banana"))
    }

    private fun String.rotate(shift: Int) = drop(shift) + take(shift)

    private fun String.wrap(start: Char, end: Char) = start + this + end
    
    private fun String.unwrap(start: Char, end: Char) =
            if (first() == start && last() == end) substring(1, length - 1) else null
}
