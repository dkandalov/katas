@file:Suppress("UnstableApiUsage")

package katas.kotlin.skiena.combinatorial_search

import com.google.common.collect.*
import kotlincommon.*
import kotlincommon.test.shouldEqual
import org.junit.Assert.fail
import org.junit.Ignore
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
            word.value.toCharArray().forEach {
                if (!it.isLetter()) fail(word.value)
            }
        }
    }

/*
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
*/

    @Test fun `find 'fairy tales' anagrams with tiny dictionary`() {
        val anagramFinder = AnagramFinder(
            text = "fairy tales".toText(),
            dictionary = listOf("rail", "safety", "fairy", "tales").map { Word(it) }
        )
        anagramFinder.hasNext() shouldEqual true
        anagramFinder.next().value shouldEqual "rail".toText()
        anagramFinder.next().hasNext() shouldEqual true
        anagramFinder.next().next().value shouldEqual "rail safety".toText()

        backtrack(anagramFinder).map { it.value.joinToString() } shouldEqual listOf(
            "tales fairy",
            "safety rail",
            "rail safety"
        )
    }

    @Ignore("because it takes a very long time so run it manually")
    @Test fun `find 'fairy tales' anagrams with full dictionary`() {
        val text = "fairy tales".toText()
        val filteredDictionary = dictionary
            .filter { it.value.length > 2 }
            .filter { word -> text.letters.containsAll(word.chars) }
        val anagramFinder = AnagramFinder(text, filteredDictionary)

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

        (ImmutableMultiset.of(1, 1, 2, 3) == ImmutableMultiset.of(1, 2, 3)) shouldEqual false
        (ImmutableMultiset.of(1, 1, 2, 3) == ImmutableMultiset.of(1, 1, 2, 3)) shouldEqual true
    }
}

private data class Word(val value: String) {
    val chars: Multiset<Char> = value.toCharMultiset()
}

private fun String.toWord(): Word = Word(normalised())

private data class Text(val words: List<Word>) {
    val letters: Multiset<Char> = words.fold(HashMultiset.create()) { acc, it ->
        acc.addAll(it.chars)
        acc
    }

    fun add(word: Word) = Text(words + word)

    fun joinToString() = words.joinToString(" ") { it.value }
}

private fun String.toText(): Text = Text(split(" ").map { it.toWord() })


private inline fun <T> List<T>.findIndexed(skipElements: Int = 0, predicate: (T) -> Boolean): Pair<Int, T?> {
    (skipElements..lastIndex).forEach { i ->
        val element = this[i]
        if (predicate(element)) return Pair(i, element)
    }
    return Pair(-1, null)
}



private data class AnagramFinder(
    val text: Text,
    val dictionary: List<Word>,
    val dictionaryIndex: Int = 0,
    override val value: Text = Text(emptyList()),
    val remainingChars: Multiset<Char> = text.letters,
    val skippedWords: Set<Word> = emptySet()
) : Solution<Text> {
    private var nextWord: Word?
    private var nextDictionaryIndex: Int?

    init {
        val (i, word) = dictionary.findIndexed(dictionaryIndex) { word ->
            skippedWords.doesNotContain(word) && remainingChars.containsAll(word.chars)
        }
        nextWord = word
        nextDictionaryIndex = i
    }

    override fun hasNext() = nextWord != null

    override fun skipNext() = copy(
        dictionaryIndex = nextDictionaryIndex!!,
        skippedWords = skippedWords + nextWord!!
    )/*.printed { it.skippedWords.size.toString() }*/

    override fun next() = copy(
        value = value.add(nextWord!!),
        dictionaryIndex = 0,
        remainingChars = Multisets.difference(remainingChars, nextWord!!.chars),
        skippedWords = emptySet()
    )/*.printed { it.value.words.first().value }*/

    override fun isComplete() = value != text && value.letters == text.letters
}

private val dictionary: Set<Word> = File("src/katas/kotlin/words.txt").readLines().map { it.toWord() }.toSet()

private fun String.toCharMultiset(): Multiset<Char> = ImmutableMultiset.copyOf(toCharArray().toTypedArray())

private fun String.normalised(): String = replace("'", "").replace("-", "").toLowerCase()

private fun String.isAnagramOf(s: String): Boolean =
    replace(" ", "").toLowerCase().toCharArray().sorted() == s.replace(" ", "").toLowerCase().toCharArray().sorted()
