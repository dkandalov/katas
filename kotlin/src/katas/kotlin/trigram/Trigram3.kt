package katas.kotlin.trigram

import io.kotlintest.specs.StringSpec
import katas.kotlin.println
import katas.kotlin.sliding
import java.io.File
import java.util.Random

class Trigram3Test : StringSpec() {
    init {
        "build trigrams from book text and generate delirious text" {
            val words = File("src/katas/kotlin/trigram/18440-0.txt").words()
            val trigrams = words.sliding(3).groupingBy { Pair(it[0], it[1]) }
                    .aggregate { _, list: List<String>?, element, _ ->
                        (list ?: emptyList()) + element.last()
                    }

            val random = Random(123)
            val initialKey = trigrams.keys.drop(random.nextInt(trigrams.size)).first()
            val textGenerator = generateSequence(initialKey) { key ->
                val nextWords = trigrams[key]
                val nextWord = nextWords!!.drop(random.nextInt(nextWords.size)).first()
                Pair(key.second, nextWord)
            }.map{ it.first }

            val text = textGenerator.take(100).joinToString(" ")
            text.println()
            text should startWith(
                    "_one_ of its denotation. If 'man,' for example, it is in the course of nature or in the mythopœic age. " +
                    "We must not be put in vague language. (6) It was shown in this, that they were, in a hole; " +
                    "so that their relations to the curve till no appreciable difference remains."
            )
        }
    }

    private fun File.words(): List<String> {
        return readLines()
            .flatMap { it.trim().trim("feff".toInt(16).toChar()).split(Regex("\\s+")) }
            .filterNot { it.isNullOrEmpty() }
    }
}