package katas.kotlin.trigram

import katas.kotlin.println
import katas.kotlin.sliding
import org.junit.Test
import java.io.File
import java.util.Random

class Trigram2Test {
    private val basePath = "src/katas/kotlin/trigram"

    @Test fun `read books and generate delirious text based on trigrams`() {
        val words = File("$basePath/18440-0.txt").readLines().flatMap{ it.splitIntoWords() }.map{ it.trim() }.filter{ it.isNotBlank() }
        val trigrams = words.sliding(3).groupingBy{ Pair(it[0], it[1]) }
                .fold(emptyList<String>()) { acc, it -> acc + it[2] }

        val random = Random(123)
        var key = trigrams.entries.toList()[random.nextInt(trigrams.size)].key
        val text = generateSequence {
            val nextWords = trigrams[key]
            if (nextWords != null) {
                val word = nextWords[random.nextInt(nextWords.size)]
                key = Pair(key.second, word)
                word
            } else {
                key = trigrams.entries.toList()[random.nextInt(trigrams.size)].key
                key.second
            }
        }

        text.take(100).joinToString(" ").println()
    }

    private fun String.splitIntoWords() = this.trim().split(" ")
}

