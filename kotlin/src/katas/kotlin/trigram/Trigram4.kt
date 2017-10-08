package katas.kotlin.trigram

import katas.kotlin.sliding
import org.junit.Test
import java.io.File
import java.util.*

class Trigram4 {
    @Test fun `create trigram map and generate delirious text`() {
        val words = File("src/katas/kotlin/trigram/18440-0.txt").readWords()
        val trigramMap = words.sliding(windowSize = 3)
            .map { Trigram(it[0], it[1], it[2]) }
            .groupBy { Pair(it.first, it.second) }

        val random = Random(123)
        val initial = trigramMap.keys.toList()[random.nextInt()]
//        generateSequence(initial) {
//            val nextWords = trigramMap[it]
//            nextWords!![random.nextInt(nextWords.size)]
//        }
    }

    private data class Trigram(val first: String, val second: String, val third: String)

    private fun File.readWords(): List<String> {
        val regex = Regex("\\s+")
        return this.readLines().flatMap { it.split(regex) }
    }
}
