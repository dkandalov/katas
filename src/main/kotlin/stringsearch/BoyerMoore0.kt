package stringsearch

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Assert.fail
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
        assertThat("a".findIndexOf(""), equalTo(0))

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

        assertThat("aabb".findIndexOf("aab"), equalTo(0))
        assertThat("aabb".findIndexOf("abb"), equalTo(1))

        assertThat("abcdabcd".findIndexOf("ab_d"), equalTo(-1))
        assertThat("abcdabcd".findIndexOf("abc_"), equalTo(-1))
        assertThat("abcdabcd".findIndexOf("a_cd"), equalTo(-1))
        assertThat("abcdabcd".findIndexOf("_bcd"), equalTo(-1))

        assertThat("oom".findIndexOf("_om"), equalTo(-1)) // case when it's possible to get into infinite loop

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

    @Test fun `bad character rule`() {
        assertCharLookupShift(
            //  ↓
            "abcdefgh",
            "xxxx",
            "----xxxx"
        )
        assertCharLookupShift(
            //      ↓
            "abcdefgh",
            "----xxxx",
            "--------xxxx"
        )
        assertCharLookupShift(
            //  ↓
            "abcdfg",
            "xxxx",
            "----xxxx"
        )

        assertCharLookupShift(
            //  ↓
            "abcdefgh",
            "xxdx",
            "-xxdx"
        )
        assertCharLookupShift(
            //  ↓
            "abcdefgh",
            "xdxx",
            "--xdxx"
        )
        assertCharLookupShift(
            //  ↓
            "abcdefgh",
            "dxxx",
            "---dxxx"
        )

        assertCharLookupShift(
            // ~~↓
            "GCTTCTGCTACCTTTTGCGCGCGCGCGGAA",
            "CCTTTTGC",
            "---CCTTTTGC"
        )
        assertCharLookupShift(
            // ~~↓
            "ANPANMANAM",
            "-NNAAMAN",
            "---NNAAMAN"
        )
    }

    @Test fun `good suffix rule (case 1)`() {
        assertOffsetLookupShift(
            // ↓~~
            "abxabcab",
            "cccab",
            "-----cccab"
        )
        assertOffsetLookupShift(
            //  ↓~~
            "xabxabcab",
            "-abcab",
            //~~
            "----abcab"
        )
        assertOffsetLookupShift(
            //    ↓~~~
            "CGTGCCTACTTACTTACTTACTTACGCGAA",
            "CTTACTTAC",
            // ~~~
            "----CTTACTTAC"
        )
    }

    @Test fun `good suffix rule (case 2)`() {
        assertOffsetLookupShift(
            //    ↓  ~~~~~
            "CGTGCCTACTTACTTACTTACTTACGCGAA",
            "----CTTACTTAC",
            //   ~~~~~
            "--------CTTACTTAC"
        )
    }

    @Test fun `char lookup`() {
        fun lookupAllIndices(mismatchChar: Char, needle: String): List<Int> {
            val charLookup = makeCharLookup(needle)
            return needle.indices.map { charLookup(mismatchChar, it) }
        }

        assertThat(lookupAllIndices('a', "abcab"), equalTo(
                listOf(0, 1, 2, 0, 1)
        ))
        assertThat(lookupAllIndices('b', "abcab"), equalTo(
                listOf(1, 0, 1, 2, 0)
        ))
        assertThat(lookupAllIndices('c', "abcab"), equalTo(
                listOf(1, 2, 0, 1, 2)
        ))
        assertThat(lookupAllIndices('x', "abcab"), equalTo(
                listOf(1, 2, 3, 4, 5)
        ))
    }

    @Test fun `prefix lookup`() {
        fun lookupAllIndices(needle: String): List<Int> {
            val offsetLookup = makePrefixOffsetLookup(needle)
            return needle.indices.map { offsetLookup(it) }
        }

        assertThat(lookupAllIndices("abcde"), equalTo(
                listOf(5, 4, 3, 2, 1)
        ))
        assertThat(lookupAllIndices("abcab"), equalTo(
                listOf(5, 3, 3, 2, 1)
        ))
        assertThat(lookupAllIndices("abcxxab"), equalTo(
                listOf(5, 5, 3, 5, 5, 2, 1)
        ))
        assertThat(lookupAllIndices("CTTACTTAC"), equalTo(
                listOf(9, 8, 9, 9, 9, 4, 9, 9, 9)
        ))
    }

    @Test fun `postfix lookup`() {
        fun lookupAllIndices(needle: String): List<Int> {
            val offsetLookup = makePostfixOffsetLookup(needle)
            return needle.indices.map { offsetLookup(it) }
        }

        assertThat(lookupAllIndices("abcde"), equalTo(
                listOf(5, 5, 5, 5, 5)
        ))
        assertThat(lookupAllIndices("abcab"), equalTo(
                listOf(5, 5, 3, 5, 5)
        ))
        assertThat(lookupAllIndices("CTTACTTAC"), equalTo(
                listOf(9, 8, 9, 9, 9, 4, 9, 9, 9)
        ))
        assertThat(lookupAllIndices("abbabab"), equalTo(
                listOf(7, 7, 5, 6, 7, 4, 7)
        ))
    }


    private fun String.findIndexOf(needle: String): Int {
        if (needle.length == 0) {
            return 0
        }
        val charLookup = makeCharLookup(needle)
        val offsetLookup = makeOffsetLookup(needle)
        var i = needle.lastIndex
        while (i < this.length) {
            val move = nextMove(this, needle, i, charLookup, offsetLookup)
            when (move) {
                is Result -> return move.i
                is Shift -> i += move.shiftForward
            }
        }
        return -1
    }

    private fun nextMove(s: String, needle: String, i: Int,
                         charLookup: (Char, Int) -> (Int), offsetLookup: (Int) -> (Int)): AMove {
        var j = needle.lastIndex
        var mi = i
        while (needle[j] == s[mi]) {
            if (j == 0) {
                return Result(mi)
            }
            --mi
            --j
        }
//        return Shift(1) // naive method
//        return Shift(Math.max(1, j - charLookup(s[i - (needle.lastIndex - j)], j)))
        val i1 = offsetLookup(j)
        val i2 = charLookup(s[i - (needle.lastIndex - j)], j)
        return Shift(Math.max(1, Math.max(i1, i2)))
    }

    private interface AMove
    private data class Result(val i: Int) : AMove
    private data class Shift(val shiftForward: Int) : AMove

    private fun makeCharLookup(needle: String): (Char, Int) -> (Int) {
        val table = HashMap<Pair<Char, Int>, Int>()
        val lastCharIndex = HashMap<Char, Int>()
        needle.forEachIndexed { i, char ->
            lastCharIndex[char] = i
            lastCharIndex.entries.forEach {
                table[Pair(it.key, i)] = it.value
            }
        }
        return { mismatchChar, mismatchIndex ->
            mismatchIndex - table.getOrDefault(Pair(mismatchChar, mismatchIndex), -1)
        }
    }

    private fun makeOffsetLookup(needle: String): (Int) -> (Int) {
        val lookup1 = makePrefixOffsetLookup(needle)
        val lookup2 = makePostfixOffsetLookup(needle)
        return { mismatchIndex ->
            val result = lookup1(mismatchIndex)
            if (result == needle.length) lookup2(mismatchIndex) else result
        }
    }

    private fun makePrefixOffsetLookup(needle: String): (Int) -> (Int) {
        val table = HashMap<Int, Int>()
        var prefixIndex = needle.length

        for (i in needle.lastIndex.downTo(0)) {
            if (isPrefix(needle, i + 1)) {
                prefixIndex = i + 1
            }
            table[i] = prefixIndex - i
        }

        return { mismatchIndex ->
            table.getOrDefault(mismatchIndex, needle.length)
        }
    }

    private fun makePostfixOffsetLookup(needle:String): (Int) -> (Int) {
        val table = HashMap<Int, Int>()
        for (i in 0..(needle.lastIndex - 1)) {
            val suffixLength = suffixLength(needle, i)
            if (suffixLength > 0) {
                table[i + 1] = needle.length - suffixLength
            }
        }
        return { mismatchIndex ->
            table.getOrDefault(mismatchIndex, needle.length)
        }
    }

    private fun isPrefix(needle: String, suffixStartIndex: Int): Boolean {
        var i = suffixStartIndex
        var j = 0
        while (i < needle.length) {
            if (needle[i] != needle[j]) return false
            ++i
            ++j
        }
        return true
    }

    private fun suffixLength(needle: String, index: Int): Int {
        var length = 0
        var i = index
        var j = needle.lastIndex
        while (i >= 0 && needle[i] == needle[j]) {
            length += 1
            --i
            --j
        }
        return length
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

    private fun assertCharLookupShift(s: String, needleBefore: String, needleAfter: String) {
        assertShift(s, needleBefore, needleAfter, true, false)
    }

    private fun assertOffsetLookupShift(s: String, needleBefore: String, needleAfter: String) {
        assertShift(s, needleBefore, needleAfter, false, true)
    }

    private fun assertShift(s: String, needleBefore: String, needleAfter: String,
                            useCharLookup: Boolean, useOffsetLookup: Boolean) {
        val needle = needleBefore.replace("-", "")
        val charLookup = if (useCharLookup) makeCharLookup(needle) else {c, i -> 1}
        val offsetLookup = if (useOffsetLookup) makeOffsetLookup(needle) else {i -> 1}
        val move = nextMove(s, needle, needleBefore.length - 1, charLookup, offsetLookup)
        if (move !is Shift) {
            failTest("Expected move to be a shift but it was $move")
        }
        val prefix = 0.until(move.shiftForward).map{ "-" }.joinToString("")
        assertThat(prefix + needleBefore, equalTo(needleAfter))
    }

    private fun failTest(reason: String): Nothing {
        fail(reason)
        throw IllegalStateException()
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

