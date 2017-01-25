package katas.kotlin.trigram

import io.kotlintest.specs.StringSpec
import katas.kotlin.println
import katas.kotlin.sliding
import java.io.File

class Trigram3Test : StringSpec() {
    init {
        "build trigrams from book text and generate delirious text" {
            val words = File("src/katas/kotlin/trigram/18440-0.txt").words()
            words.sliding(3).groupingBy { Pair(it[0], it[1]) }
                    .aggregate { _, _: List<String>?, element, _ -> element }
                    .entries.associate { Pair(it.key, it.value.drop(2)) }
                    .entries.filter{it.value.size > 1}.take(10).println()

            TODO()
        }
    }

    private fun File.words(): List<String> {
        return readLines()
            .flatMap { it.trim().trim("feff".toInt(16).toChar()).split(Regex("\\s+")) }
            .filterNot { it.isNullOrEmpty() }
    }
}