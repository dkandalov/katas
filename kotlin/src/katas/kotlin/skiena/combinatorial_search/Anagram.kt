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
        fun String.isTextAnagramOf(that: String) = this.toText().isAnagramOf(that.toText())

        "".isTextAnagramOf("") shouldEqual false
        "a".isTextAnagramOf("a") shouldEqual false
        "a".isTextAnagramOf("b") shouldEqual false
        "b".isTextAnagramOf("a") shouldEqual false

        "ab".isTextAnagramOf("ba") shouldEqual true
        "ba".isTextAnagramOf("ab") shouldEqual true
        "ab".isTextAnagramOf("abb") shouldEqual false

        "a b".isTextAnagramOf("ab") shouldEqual true
        "a b".isTextAnagramOf("ba") shouldEqual true
        "AB".isTextAnagramOf("ba") shouldEqual true

        "listen".isTextAnagramOf("silent") shouldEqual true
        "rail safety".isTextAnagramOf("fairy tales") shouldEqual true
        "customers".isTextAnagramOf("store scum") shouldEqual true
        "William Shakespeare".isTextAnagramOf("I am a weakish speller") shouldEqual true
    }

    @Test fun `dictionary contains words with letters only`() {
        defaultDictionary.forEach { word ->
            word.value.toCharArray().forEach {
                if (!it.isLetter()) fail(word.value)
            }
        }
    }

    @Test fun `find 'fairy tales' anagrams with tiny dictionary`() {
        val anagramFinder = AnagramFinder(
            text = "fairy tales".toText(),
            dictionary = listOf("rail", "safety", "fairy", "tales").map { Word(it) }
        )
        anagramFinder.hasNext() shouldEqual true
        anagramFinder.next().value shouldEqual "rail".toText()
        anagramFinder.next().hasNext() shouldEqual true
        anagramFinder.next().next().value shouldEqual "rail safety".toText()

        backtrack(anagramFinder).map { it.value.toPrintableString() } shouldEqual listOf(
            "rail safety"
        )
    }

    @Test fun `find anagrams with duplicate words`() {
        val dictionary = listOf("aaa", "fun", "nuf", "unf", "zzz").map { Word(it) }
        findAnagrams("fun fun", dictionary) shouldEqual listOf(
            "unf unf",
            "nuf unf",
            "nuf nuf",
            "fun unf",
            "fun nuf"
        )
    }

    @Ignore("because it takes long time so run it manually")
    @Test fun `find 'fairy tales' anagrams with full dictionary`() {
        findAnagrams("fairy tales").forEach { it.printed() }
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

private fun findAnagrams(s: String, dictionary: List<Word> = defaultDictionary): List<String> {
    val text = s.toText()
    val filteredDictionary = dictionary
        .filter { it.value.length > 2 }
        .filter { word -> text.letters.containsAll(word.chars) }

    val anagramFinder = AnagramFinder(text, filteredDictionary)

    return backtrack(anagramFinder).map { it.value.toPrintableString() }
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
    private var nextDictionaryIndex: Int

    init {
        val (i, word) = dictionary.findIndexed(dictionaryIndex) { word ->
            skippedWords.doesNotContain(word) && remainingChars.containsAll(word.chars)
        }
        nextWord = word
        nextDictionaryIndex = i
    }

    override fun hasNext() = nextWord != null

    override fun skipNext() = copy(
        dictionaryIndex = nextDictionaryIndex,
        skippedWords = skippedWords + nextWord!!
    )/*.printed { it.skippedWords.size.toString() }*/

    override fun next() = copy(
        value = value.add(nextWord!!),
        dictionaryIndex = nextDictionaryIndex, // By updating index we exclude from output permutations of words, e.g. only one of "ab cd" and "cd ab" will be returned
        remainingChars = Multisets.difference(remainingChars, nextWord!!.chars),
        skippedWords = emptySet()
    )/*.printed { it.value.words.first().value }*/

    override fun isComplete() = value.isAnagramOf(text)
}


private data class Word(val value: String) {
    val chars: Multiset<Char> = value.toCharMultiset()
}

private data class Text(val words: List<Word>) {
    val letters: Multiset<Char> = words.fold(HashMultiset.create()) { acc, it ->
        acc.addAll(it.chars)
        acc
    }

    fun add(word: Word) = Text(words + word)

    fun isAnagramOf(text: Text) = this != text && letters == text.letters

    fun toPrintableString() = words.joinToString(" ") { it.value }
}

private fun String.toWord() = Word(normalised())

private fun String.toText() = Text(split(" ").map { it.toWord() })

private inline fun <T> List<T>.findIndexed(skipElements: Int = 0, predicate: (T) -> Boolean): Pair<Int, T?> {
    (skipElements..lastIndex).forEach { i ->
        val element = this[i]
        if (predicate(element)) return Pair(i, element)
    }
    return Pair(-1, null)
}

private val defaultDictionary: List<Word> = File("src/katas/kotlin/words.txt").readLines().map { it.toWord() }

private fun String.toCharMultiset(): Multiset<Char> = ImmutableMultiset.copyOf(toCharArray().toTypedArray())

private fun String.normalised(): String = replace("'", "").replace("-", "").toLowerCase()
