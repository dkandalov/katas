package katas.kotlin.skiena.combinatorial_search

import junit.framework.Assert.fail
import kotlincommon.join
import kotlincommon.permutationsSequence
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.io.File

class AnagramTests {
    @Test fun `check if strings are anagrams`() {
        "".isAnagramOf("") shouldEqual true
        "a".isAnagramOf("a") shouldEqual true

        "a".isAnagramOf("b") shouldEqual false
        "b".isAnagramOf("a") shouldEqual false

        "ab".isAnagramOf("ba") shouldEqual true
        "ba".isAnagramOf("ab") shouldEqual true
        "ab".isAnagramOf("abb") shouldEqual false

        "a b".isAnagramOf("ab") shouldEqual true
        "a b".isAnagramOf("ba") shouldEqual true
        "AB".isAnagramOf("ba") shouldEqual true

        "listen".isAnagramOf("silent") shouldEqual true
        "rail safety".isAnagramOf("fairy tales") shouldEqual true
        "customers".isAnagramOf("store scum") shouldEqual true
        "William Shakespeare".isAnagramOf("I am a weakish speller") shouldEqual true
    }

    @Test fun `find anagrams based on a dictionary`() {
        fun String.normalised(): String = replace("'", "").replace("-", "").toLowerCase()

        val words = File("src/katas/kotlin/words.txt").readLines().map { it.normalised() }.toSet()

        words.forEach { word ->
            word.toCharArray().forEach {
                if (!it.isLetter()) fail(word)
            }
        }

        fun String.findAnagrams(): List<String> {
            val normalisedText = normalised()
            return normalisedText.replace(" ", "")
                .toCharArray().toList().permutationsSequence()
                .map { it.join("") }
                .filter { it != normalisedText && words.contains(it) }
                .toList()
        }

        "listen".findAnagrams() shouldEqual listOf("silent", "tinsel", "enlist")
        "lis ten".findAnagrams() shouldEqual listOf("listen", "silent", "tinsel", "enlist")
        "Listen".findAnagrams() shouldEqual listOf("silent", "tinsel", "enlist")
    }
}

private fun String.isAnagramOf(s: String): Boolean =
    replace(" ", "").toLowerCase().toCharArray().sorted() == s.replace(" ", "").toLowerCase().toCharArray().sorted()
