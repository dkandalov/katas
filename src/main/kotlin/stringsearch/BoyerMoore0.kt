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
            "-cccab"
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

    @Test fun `shift on char mismatch`() {
        fun assertShifts(needle: String, vararg mismatchAndExpectedShift: Pair<String, Int>) {
            val lookupShift = makeCharShiftLookup(needle)
            val actualShifts = mismatchAndExpectedShift
                .map { it.first }
                .map { mismatch ->
                    val mismatchChar = mismatch.trim()[0]
                    val mismatchIndex = mismatch.takeWhile{ it == ' ' }.count()
                    lookupShift(mismatchChar, mismatchIndex)
                }
            val expectedShifts = mismatchAndExpectedShift.map{ it.second }
            assertThat(actualShifts, equalTo(expectedShifts))
        }

        assertShifts("abcab",
                Pair("    a", 1),
                Pair("   a ", 0),
                Pair("  a  ", 2),
                Pair(" a   ", 1),
                Pair("a    ", 0)
        )
        assertShifts("abcab",
                Pair("    b", 0),
                Pair("   b ", 2),
                Pair("  b  ", 1),
                Pair(" b   ", 0),
                Pair("b    ", 1)
        )
        assertShifts("abcab",
                Pair("    c", 2),
                Pair("   c ", 1),
                Pair("  c  ", 0),
                Pair(" c   ", 2),
                Pair("c    ", 1)
        )
        assertShifts("abcab",
                Pair("    x", 5),
                Pair("   x ", 4), // TODO shift 5
                Pair("  x  ", 3),
                Pair(" x   ", 2),
                Pair("x    ", 1)
        )
    }

    @Test fun `shift on prefix lookup`() {
        fun shiftsOnMismatch(needle: String): List<Int> {
            val lookupShift = makePrefixShiftLookup(needle)
            return needle.indices.map { lookupShift(it) }
        }
        fun assertShift(mismatch: String, needle: String, expectedShiftedNeedle: String) {
            val lookupShift = makePrefixShiftLookup(needle)

            val mismatchIndex = mismatch.takeWhile{ it == ' ' }.count()
            val shift = lookupShift(mismatchIndex)
            val actualShiftedNeedle = needle.padStart(needle.length + shift, ' ')

            assertThat(actualShiftedNeedle, equalTo(expectedShiftedNeedle))
        }

        assertShift(
                "    ↓",
                "abcab",
                "abcab")
        assertShift(
                "   ↓ ",
                "abcab",
                "abcab")
        assertShift(
                "  ↓  ",
                "abcab",
                "   abcab")
        assertShift(
                " ↓   ",
                "abcab",
                "   abcab")
        assertShift(
                "↓    ",
                "abcab",
                "abcab")
        assertThat(shiftsOnMismatch("abcab"), equalTo(
                listOf(0, 3, 3, 0, 0)
        ))

        assertThat(shiftsOnMismatch("abcde"), equalTo(
                listOf(0, 0, 0, 0, 0)
        ))
        assertThat(shiftsOnMismatch("abcxxab"), equalTo(
                listOf(0, 5, 5, 5, 5, 0, 0)
        ))
        assertThat(shiftsOnMismatch("CTTACTTAC"), equalTo(
                listOf(0, 4, 4, 4, 8, 8, 8, 8, 0) // TODO
        ))
    }

    @Test fun `shift on suffix lookup`() {
        fun shiftsOnMismatch(needle: String): List<Int> {
            val lookupShift = makeSuffixShiftLookup(needle)
            return needle.indices.map { lookupShift(it) }
        }
        fun assertShift(mismatch: String, needle: String, expectedShiftedNeedle: String) {
            val lookupShift = makeSuffixShiftLookup(needle)

            val mismatchIndex = mismatch.takeWhile{ it == ' ' }.count()
            val shift = lookupShift(mismatchIndex)
            val actualShiftedNeedle = needle.padStart(needle.length + shift, ' ')

            assertThat(actualShiftedNeedle, equalTo(expectedShiftedNeedle))
        }

        assertShift(
                "    ↓",
                "abcab",
                "abcab")
        assertShift(
                "   ↓ ",
                "abcab",
                "abcab")
        assertShift(
                "  ↓  ",
                "abcab",
                "   abcab")
        assertShift(
                " ↓   ",
                "abcab",
                "abcab")
        assertShift(
                "↓    ",
                "abcab",
                "abcab")


        assertShift(
                " ↓     ",
                "abbabab",
                "abbabab")
        assertShift(
                "  ↓    ",
                "abbabab",
                "   abbabab")
        assertShift(
                "   ↓   ",
                "abbabab",
                "  abbabab")
        assertShift(
                "    ↓  ",
                "abbabab",
                "abbabab")
        assertShift(
                "     ↓ ",
                "abbabab",
                "  abbabab")

        assertThat(shiftsOnMismatch("abcde"), equalTo(
                listOf(0, 0, 0, 0, 0)
        ))
        assertThat(shiftsOnMismatch("abbabab"), equalTo(
                listOf(0, 0, 3, 2, 0, 2, 0)
        ))
        assertThat(shiftsOnMismatch("CTTACTTAC"), equalTo(
                listOf(0, 2, 0, 0, 0, 4, 0, 0, 0) // TODO 2?
        ))
    }


    private fun String.findIndexOf(needle: String): Int {
        if (needle.length == 0) return 0

        val charLookup = makeCharShiftLookup(needle)
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

    private fun makeCharShiftLookup(needle: String): (Char, Int) -> (Int) {
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
        val lookup1 = makePrefixShiftLookup(needle)
        val lookup2 = makeSuffixShiftLookup(needle)
        return { mismatchIndex ->
            val result1 = lookup1(mismatchIndex)
            val result2 = lookup2(mismatchIndex)
            if (result1 == 0) {
                result2
            } else if (result2 == 0) {
                result1
            } else {
                Math.min(result1, result2)
            }
        }
    }

    private fun makePrefixShiftLookup(needle: String): (Int) -> (Int) {
        val table = HashMap<Int, Int>()
        var prefixIndex = 0

        for (mismatchIndex in (needle.lastIndex - 1).downTo(1)) {
            if (isPrefix(needle, mismatchIndex)) {
                prefixIndex = mismatchIndex + 1
            }
            table[mismatchIndex] = prefixIndex
        }

        return { mismatchIndex ->
            table.getOrDefault(mismatchIndex, 0)
        }
    }

    private fun makeSuffixShiftLookup(needle: String): (Int) -> (Int) {
        val table = HashMap<Int, Int>()
        for (mismatchIndex in 1..(needle.lastIndex - 1)) {
            val suffixLength = suffixLength(needle, mismatchIndex)
            if (suffixLength > 0) {
                table[mismatchIndex] = suffixLength + 1 // +1 to include mismatched character
            }
        }
        return { mismatchIndex ->
            table.getOrDefault(mismatchIndex, 0)
        }
    }

    private fun isPrefix(needle: String, mismatchIndex: Int): Boolean {
        var i = mismatchIndex + 1
        var j = 0
        while (i < needle.length) {
            if (needle[i] != needle[j]) return false
            ++i
            ++j
        }
        return true
    }

    private fun suffixLength(needle: String, mismatchIndex: Int): Int {
        var length = 0
        var i = mismatchIndex - 1
        var j = needle.lastIndex
        while (i >= 0 && j > mismatchIndex && needle[i] == needle[j]) {
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
        val charLookup = if (useCharLookup) makeCharShiftLookup(needle) else { c, i -> 1}
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

