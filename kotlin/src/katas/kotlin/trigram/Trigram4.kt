package katas.kotlin.trigram

import org.junit.Test
import java.io.File
import java.util.*

class Trigram4 {
    
    @Test fun `create trigram map and generate delirious text`() {
        val words = listOf(
            File("src/katas/kotlin/trigram/18440-0.txt"),
            File("src/katas/kotlin/trigram/39702-0.txt"),
            File("src/katas/kotlin/trigram/53970-0.txt")
        ).flatMap{ it.readWords() }
        val trigramMap = words.windowed(size = 3)
            .map { Trigram(it[0], it[1], it[2]) }
            .groupBy { Pair(it.first.normalise(), it.second.normalise()) }

        val random = Random(123)
        val initial = trigramMap.values.elementAt(random.nextInt(trigramMap.size)).let {
            it[random.nextInt(it.size)]
        }
        val trigrams = generateSequence(initial) {
            val nextWords = trigramMap[Pair(it.second.normalise(), it.third.normalise())]!!
            nextWords[random.nextInt(nextWords.size)]
        }
        trigrams.map{ it.third }
            .take(200)
            .forEach {
                print(it + " ")
            }
    }

    private data class Trigram(val first: String, val second: String, val third: String)

    private fun String.normalise(): String {
        return this.trim()
            .toLowerCase()
            .replace("-", "")
            .replace("_", "")
            .replace(",", "")
            .replace(";", "")
            .replace(":", "")
            .replace(".", "")
            .replace("!", "")
            .replace("?", "")
            .replace("'", "")
    }

    private fun File.readWords(): List<String> {
        val regex = Regex("\\s+")
        return this.readLines()
            .flatMap { it.split(regex) }
            .map{ it.replace("_", "") }
    }
}
