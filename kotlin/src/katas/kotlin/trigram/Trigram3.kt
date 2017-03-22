package katas.kotlin.trigram

import io.kotlintest.specs.StringSpec
import katas.kotlin.println
import katas.kotlin.skip
import katas.kotlin.sliding
import java.io.File
import java.util.*

class Trigram3Test : StringSpec() {
    init {
        "build trigrams from book text and generate delirious text" {
            val words = File("src/katas/kotlin/trigram/18440-0.txt").words()
            val trigrams = words.sliding(3).groupingBy { Pair(it[0], it[1]) }
                .aggregate { _, list: List<String>?, element, _ ->
                    (list ?: emptyList()) + element.last()
                }

            val random = Random(123)
            val initialKey = trigrams.keys[random]
            val textGenerator = generateSequence(initialKey) { key ->
                val nexWords = trigrams[key] ?: trigrams.entries[random].value
                Pair(key.second, nexWords[random])
            }.map { it.first }

            val text = textGenerator.take(100).joinToString(" ")
            text.println()
            text should startWith(
                "_one_ of its denotation. If 'man,' for example, it is in the course of nature or in the mythopœic age. " +
                "We must not be put in vague language. (6) It was shown in this, that they were, in a hole; " +
                "so that their relations to the curve till no appreciable difference remains."
            )
        }
    }

    private operator fun <E> Collection<E>.get(random: Random): E {
        return skip(random.nextInt(size)).first()
    }

    private fun File.words(): List<String> {
        val nbsp = "feff".toInt(16).toChar()
        return readLines()
            .flatMap { it.trim().trim(nbsp).split(Regex("\\s+")) }
            .filterNot { it.isNullOrEmpty() }
    }
}