package stringsearch

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.util.*

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

    @Test fun `Boyer-Moore search`() {
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

        assertThat("abcdabcd".findIndexOf("abCd"), equalTo(-1))
        assertThat("abcdabcd".findIndexOf("abcD"), equalTo(-1))
        assertThat("abcdabcd".findIndexOf("aBcd"), equalTo(-1))
        assertThat("abcdabcd".findIndexOf("Abcd"), equalTo(-1))
        assertThat("There would have been a time for such a word".findIndexOf("word"), equalTo(-1))

        val s = "peter piper picked a peck of pickled peppers".replace(" ", "")
        assertThat(s.findIndexOf("picnic"), equalTo(-1))
        assertThat(s.findIndexOf("pickle"), equalTo(23))

        val alphabet = 'a'.rangeTo('z').toList()
        val random = Random(123)
        val text = 0.until(100).map{ alphabet[random.nextInt(alphabet.size)] }.joinToString("")
        println("text = $text")

        1.until(text.length).map { patternSize ->
            val patterns = text.sliding(patternSize)
            patterns.forEach { pattern ->
                println("pattern = $pattern")
                assertThat(text.findIndexOf(pattern), equalTo(text.indexOf(pattern)))
            }
            val notMatchingPatterns = patterns.map { it + "_" } + patterns.map { "_" + it }
            notMatchingPatterns.forEach { pattern ->
                println("pattern = $pattern")
                assertThat(text.findIndexOf(pattern), equalTo(-1))
            }
        }
    }

    @Test fun `sliding list`() {
        assertThat(listOf(1).sliding(1), equalTo(listOf(listOf(1))))

        assertThat(listOf(1, 2, 3).sliding(1), equalTo(listOf(
                listOf(1), listOf(2), listOf(3)
        )))
        assertThat(listOf(1, 2, 3).sliding(2), equalTo(listOf(
                listOf(1, 2), listOf(2, 3)
        )))
        assertThat(listOf(1, 2, 3).sliding(1), equalTo(listOf(
                listOf(1), listOf(2), listOf(3)
        )))
        assertThat(listOf(1, 2, 3).sliding(3), equalTo(listOf(
                listOf(1, 2, 3)
        )))
    }

    private fun String.findIndexOf(needle: String): Int {
        if (needle.length == 0) {
            return 0
        }
        val charTable = makeCharTable(needle)
        val offsetTable = makeOffsetTable(needle)
        var i = needle.lastIndex
        while (i < this.length) {
            var j = needle.lastIndex
            while (needle[j] == this[i]) {
                if (j == 0) {
                    return i
                }
                --i
                --j
            }
//            i += 1 // For naive method (when i is not decremented)
            i += charTable(this[i]) // TODO make this work
//            i += Math.max(offsetTable[needle.lastIndex - j], charTable(this[i]))
        }
        return -1
    }

    private fun makeCharTable(needle: String): (Char) -> (Int) {
        val table = HashMap<Char, Int>()
        for (i in 0..needle.lastIndex - 1) {
            table[needle[i]] = needle.lastIndex - i
        }
        return { char -> table.getOrDefault(char, needle.length) }
    }

    private fun makeOffsetTable(needle: String): IntArray {
        val table = IntArray(needle.length)
        var lastPrefixPosition = needle.length
        for (i in needle.indices.reversed()) {
            if (isPrefix(needle, i + 1)) {
                lastPrefixPosition = i + 1
            }
            table[needle.lastIndex - i] = lastPrefixPosition - i + needle.lastIndex
        }
        for (i in 0..needle.lastIndex - 1) {
            val slen = suffixLength(needle, i)
            table[slen] = needle.lastIndex - i + slen
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
        var j = needle.lastIndex
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

    private fun String.sliding(n: Int): List<String> {
        return this.asSequence().toList().sliding(n).map{ it.joinToString("") }
    }

    private fun <T> List<T>.sliding(n: Int): List<List<T>> {
        if (n > size) throw IllegalArgumentException()
        var i = 0
        return object : Iterator<List<T>> {
            override fun hasNext(): Boolean {
                return i + n <= size
            }

            override fun next(): List<T> {
                val result = subList(i, i + n)
                i++
                return result
            }
        }.asSequence().toList()
    }
}

