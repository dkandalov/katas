package katas.kotlin.datamunging

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.io.File
import java.lang.Integer.parseInt

class DataMunging14 {
    private val weatherFile = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"
    private val footballFile = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"

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
            .minBy { Math.abs(it!!.value1 - it.value2) }
    }

    private fun parse(line: String, idIndex: Int, valueIndex1: Int, valueIndex2: Int): Data? {
        val list = line.trim().replace("*", "").split(Regex("\\s+"))
        if (listOf(idIndex, valueIndex1, valueIndex2).max()!! >= list.size) return null
        try {
            return Data(list[idIndex], parseInt(list[valueIndex1]), parseInt(list[valueIndex2]))
        } catch(e: NumberFormatException) {
            return null
        }
    }

    private data class Data(val name: String, val value1: Int, val value2: Int)
}