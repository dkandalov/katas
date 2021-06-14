package katas.kotlin.datamunging

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.io.File
import java.lang.Integer.parseInt

class DataMunging14 {
    private val weatherFile = "src/katas/kotlin/datamunging/weather.dat"
    private val footballFile = "src/katas/kotlin/datamunging/football.dat"

    @Test fun `find day with minimum temperature spread`() {
        assertThat(parseAndFindMinValue(weatherFile, 0, 1, 2), equalTo(Data("14", 61, 59)))
    }

    @Test fun `find team with minimum goal difference`() {
        assertThat(parseAndFindMinValue(footballFile, 1, 6, 8), equalTo(Data("Aston_Villa", 46, 47)))
    }

    private fun parseAndFindMinValue(fileName: String, idIndex: Int, valueIndex1: Int, valueIndex2: Int): Data? {
        return File(fileName).readLines()
            .map { parse(it, idIndex, valueIndex1, valueIndex2) }
            .filter { it != null }
            .minByOrNull { it: Data? -> Math.abs(it!!.value1 - it.value2) }
    }

    private fun parse(line: String, idIndex: Int, valueIndex1: Int, valueIndex2: Int): Data? {
        val list = line.trim().replace("*", "").split(Regex("\\s+"))
        if (listOf(idIndex, valueIndex1, valueIndex2).maxOrNull()!! >= list.size) return null
        try {
            return Data(list[idIndex], parseInt(list[valueIndex1]), parseInt(list[valueIndex2]))
        } catch(e: NumberFormatException) {
            return null
        }
    }

    private data class Data(val name: String, val value1: Int, val value2: Int)
}