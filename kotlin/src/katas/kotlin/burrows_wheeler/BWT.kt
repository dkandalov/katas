package katas.kotlin.burrows_wheeler

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlincommon.join
import org.junit.Test

class BTW0_Test {
    @Test fun `encode and decode "banana"`() {
        assertThat(encode("banana"), equalTo("|bnn^aaa"))
        assertThat(decode("|bnn^aaa"), equalTo("banana"))

        assertThat(encode("SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"), equalTo(
                "TEXYDST.E.IXIXIXXSSMPPS.B..E.^.UESFXDIIOIIIT|S"
        ))
        assertThat(decode("TEXYDST.E.IXIXIXXSSMPPS.B..E.^.UESFXDIIOIIIT|S"), equalTo(
                "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"
        ))
    }

    private fun encode(string: String): String {
        fun doEncode(s: String): String {
            val rotations = s.indices.map { shift -> s.rotate(shift) }
            val sortedRotations = rotations.sorted()
            return sortedRotations.map { it.last() }.join("")
        }
        return doEncode("^$string|")
    }

    private fun decode(s: String): String {
        val table = MutableList(s.length, { MutableList(s.length, {0.toChar()}) })
        0.until(s.length).forEach {
            s.indices.forEach { table[it].push(s[it]) }
            table.sortBy { it.first() }
        }
        val i = table.indexOfFirst { it.first() == '^' && it.last() == '|' }
        return table[i].drop(1).dropLast(1).join("")
    }

    private fun <T> MutableList<T>.push(element: T) {
        add(0, element)
        removeAt(size - 1)
    }

    private fun String.rotate(shift: Int) = substring(shift) + substring(0, shift)
} 