package stringsearch

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test

class BoyerMoore0 {
    @Test fun `naive search`() {
        assertThat("".naiveIndexOf(""), equalTo(0))
        assertThat("abc".naiveIndexOf(""), equalTo(0))

        assertThat("abc".naiveIndexOf("-"), equalTo(-1))
        assertThat("abc".naiveIndexOf("a"), equalTo(0))
        assertThat("abc".naiveIndexOf("b"), equalTo(1))
        assertThat("abc".naiveIndexOf("c"), equalTo(2))
        assertThat("abc".naiveIndexOf("d"), equalTo(-1))

        assertThat("abc".naiveIndexOf("ab"), equalTo(0))
        assertThat("abc".naiveIndexOf("bc"), equalTo(1))
        assertThat("abc".naiveIndexOf("bca"), equalTo(-1))

        assertThat("abc".naiveIndexOf("abc"), equalTo(0))
        assertThat("abc".naiveIndexOf("abcd"), equalTo(-1))

        val s = "peter piper picked a peck of pickled peppers".replace(" ", "")
        assertThat(s.naiveIndexOf("picnic"), equalTo(-1))
        assertThat(s.naiveIndexOf("pickle"), equalTo(23))
    }

    @Test fun `Boyer Moore search`() {
        assertThat("".findIndexOf(""), equalTo(0))
        assertThat("abc".findIndexOf(""), equalTo(0))

        assertThat("abc".findIndexOf("-"), equalTo(-1))
        assertThat("abc".findIndexOf("a"), equalTo(0))
        assertThat("abc".findIndexOf("b"), equalTo(1))
        assertThat("abc".findIndexOf("c"), equalTo(2))
        assertThat("abc".findIndexOf("d"), equalTo(-1))

        assertThat("abc".findIndexOf("ab"), equalTo(0))
        assertThat("abc".findIndexOf("bc"), equalTo(1))
        assertThat("abc".findIndexOf("bca"), equalTo(-1))

        assertThat("abc".findIndexOf("abc"), equalTo(0))
        assertThat("abc".findIndexOf("abcd"), equalTo(-1))

        val s = "peter piper picked a peck of pickled peppers".replace(" ", "")
        assertThat(s.findIndexOf("picnic"), equalTo(-1))
        assertThat(s.findIndexOf("pickle"), equalTo(23))
    }


    private fun String.findIndexOf(needle: String): Int {
        if (needle.length == 0) {
            return 0
        }
        val charTable = makeCharTable(needle)
        val offsetTable = makeOffsetTable(needle)
        var i = needle.length - 1
        while (i < this.length) {
            var j = needle.length - 1
            while (needle[j] == this[i]) {
                if (j == 0) {
                    return i
                }
                --i
                --j
            }
            // i += needle.length - j; // For naive method
            i += Math.max(offsetTable[needle.length - 1 - j], charTable[this[i].toInt()])
        }
        return -1
    }

    private fun makeCharTable(needle: String): IntArray {
        val ALPHABET_SIZE = 256
        val table = IntArray(ALPHABET_SIZE)
        for (i in table.indices) {
            table[i] = needle.length
        }
        for (i in 0..needle.length - 2) {
            table[needle[i].toInt()] = needle.length - 1 - i
        }
        return table
    }

    private fun makeOffsetTable(needle: String): IntArray {
        val table = IntArray(needle.length)
        var lastPrefixPosition = needle.length
        for (i in needle.indices.reversed()) {
            if (isPrefix(needle, i + 1)) {
                lastPrefixPosition = i + 1
            }
            table[needle.length - 1 - i] = lastPrefixPosition - i + needle.length - 1
        }
        for (i in 0..needle.length - 2) {
            val slen = suffixLength(needle, i)
            table[slen] = needle.length - 1 - i + slen
        }
        return table
    }

    private fun isPrefix(needle: String, p: Int): Boolean {
        var i = p
        var j = 0
        while (i < needle.length) {
            if (needle[i] != needle[j]) return false
            ++i
            ++j
        }
        return true
    }

    private fun suffixLength(needle: String, p: Int): Int {
        var len = 0
        var i = p
        var j = needle.length - 1
        while (i >= 0 && needle[i] == needle[j]) {
            len += 1
            --i
            --j
        }
        return len
    }

    private fun String.naiveIndexOf(s: String): Int {
        if (s.isEmpty()) return 0

        var i = 0
        while (i < this.length) {
            var j = 0
            while ((i + j) < this.length && j < s.length && this[i + j] == s[j]) {
                j++
            }
            if (j == s.length) return i
            i++
        }
        return -1
    }
}

