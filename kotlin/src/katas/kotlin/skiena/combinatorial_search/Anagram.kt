@file:Suppress("UnstableApiUsage")

package katas.kotlin.skiena.combinatorial_search

import com.google.common.collect.*
import kotlincommon.doesNotContain
import kotlincommon.join
import kotlincommon.permutationsSequence
import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Assert.fail
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

    @Test fun `dictionary contains words with letters only`() {
        dictionary.forEach { word ->
            word.toCharArray().forEach {
                if (!it.isLetter()) fail(word)
            }
        }
    }

    @Test fun `find anagrams for a single word`() {
        fun String.findAnagrams(): List<String> {
            val normalisedText = normalised()
            return normalisedText.replace(" ", "")
                .toCharArray().toList().permutationsSequence()
                .map { it.join("") }
                .filter { it != normalisedText && dictionary.contains(it) }
                .toList()
        }

        "listen".findAnagrams() shouldEqual listOf("silent", "tinsel", "enlist")
        "lis ten".findAnagrams() shouldEqual listOf("listen", "silent", "tinsel", "enlist")
        "Listen".findAnagrams() shouldEqual listOf("silent", "tinsel", "enlist")
    }

    @Test fun `find 'fairy tales' anagrams with tiny dictionary`() {
        val anagramFinder = AnagramFinder(
            text = "fairy tales".normalised().replace(" ", ""),
            dictionary = setOf("rail", "safety", "fairy", "tales")
        )
        anagramFinder.hasNext() shouldEqual true
        anagramFinder.next().value shouldEqual "rail"
        anagramFinder.next().hasNext() shouldEqual true
        anagramFinder.next().next().value shouldEqual "rail safety"

        backtrack(anagramFinder).map { it.value } shouldEqual listOf(
            "tales fairy",
            "safety rail",
            "rail safety"
        )
    }

    @Test fun `find 'fairy tales' anagrams with full dictionary`() {
        val anagramFinder = AnagramFinder(
            text = "fairy tales".normalised().replace(" ", ""),
            dictionary = dictionary.filter { it.length > 2 }.toSet()
        )

        backtrack(anagramFinder).map { it.value } shouldEqual listOf(
            "tales fairy",
            "safety rail",
            "rail safety"
        )
    }

    @Test fun `multiset operations`() {
        Multisets.difference(
            ImmutableMultiset.of(1, 1, 2, 3),
            ImmutableMultiset.of(1, 2)
        ) shouldEqual ImmutableMultiset.of(1, 3)
    }
}

private data class AnagramFinder(
    val text: String,
    val dictionary: Set<String>,
    override val value: String = "",
    val chars: Multiset<Char> = text.toCharMultiset(),
    val skippedWords: Set<String> = emptySet()
) : Solution<String> {
    private val nextWord = dictionary.find { skippedWords.doesNotContain(it) && chars.containsAll(it.toCharMultiset()) }

    override fun hasNext() = nextWord != null

    override fun skipNext() = copy(skippedWords = skippedWords + nextWord!!).printed { it.skippedWords.size.toString() }

    override fun next() = copy(
        value = "$value $nextWord".trim(),
        chars = Multisets.difference(chars, nextWord!!.toCharMultiset()),
        skippedWords = emptySet()
    ).printed { it.value }

    override fun isComplete() = value.replace(" ", "") != text && value.isAnagramOf(text)
}

private val dictionary = File("src/katas/kotlin/words.txt").readLines().map { it.normalised() }.toSet()

private fun String.toCharMultiset(): Multiset<Char> = ImmutableMultiset.copyOf(toCharArray().toTypedArray())

private fun String.normalised(): String = replace("'", "").replace("-", "").toLowerCase()

private fun String.isAnagramOf(s: String): Boolean =
    replace(" ", "").toLowerCase().toCharArray().sorted() == s.replace(" ", "").toLowerCase().toCharArray().sorted()
