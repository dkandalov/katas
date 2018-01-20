package katas.kotlin.trigram

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.startsWith
import org.junit.Test
import java.io.File
import java.util.*

class Trigram2Test {
    private val basePath = "src/katas/kotlin/trigram"

    @Test fun `read books and generate delirious text based on trigrams`() {
        val words = File("$basePath/18440-0.txt").readLines().flatMap{ it.splitIntoWords() }.map{ it.trim() }.filter{ it.isNotBlank() }
        val trigrams = words.windowed(3).groupingBy{ Pair(it[0], it[1]) }
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

        assertThat(text.take(100).joinToString(" "), startsWith(
            "but only of a given event. But how is such that should it not only what is lost in elegance is an instance " +
            "(which we have a double relation. In denotation the genus includes the genus. To take Mill's example, " +
            "if _Socrates is a question of general production; or because, if there were men to observe them, " +
            "and they remain in their opposite relations to one pound-degree Fahrenheit for every argument shall refer, " +
            "then, any propositions whose predicates do not at all these philosophers are not really exist on their account. " +
            "On the other premise, we get-- Some things friendly"
        ))
    }

    private fun String.splitIntoWords() = this.trim().split(" ")
}

