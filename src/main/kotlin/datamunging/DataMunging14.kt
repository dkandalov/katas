package datamunging

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.io.File
import java.lang.Integer.parseInt

class DataMunging {
    private val weatherFile = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"
    private val footballFile = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"

    @Test fun `find day with minimum temperature spread`() {
        val day = File(weatherFile).readLines()
            .map{ parseWeatherFileLine(it) }
            .filter{ it != null }
            .minBy{ it!!.max - it.min }
        assertThat(day, equalTo(Day(14, 59, 61)))
    }

    @Test fun `find team with minimum goal difference`() {
        val team = File(footballFile).readLines()
            .map{ parseFootballFileLine(it)}
            .filter{ it != null }
            .minBy{ Math.abs(it!!.goalsFor - it.goalsAgainst) }
        assertThat(team, equalTo(Team("Aston_Villa", 46, 47)))
    }

    private fun parseFootballFileLine(line: String): Team? {
        val list = line.trim().split(Regex("\\s+"))
        if (list.size < 9) return null
        try {
            return Team(list[1], parseInt(list[6]), parseInt(list[8]))
        } catch(e: NumberFormatException) {
            return null
        }
    }

    private fun parseWeatherFileLine(line: String): Day? {
        val list = line.trim().replace("*", "").split(Regex("\\s+"))
        if (list.size < 3) return null
        try {
            return Day(parseInt(list[0]), parseInt(list[2]), parseInt(list[1]))
        } catch(e: NumberFormatException) {
            return null
        }
    }

    private data class Day(val id: Int, val min: Int, val max: Int)
    private data class Team(val name: String, val goalsFor: Int, val goalsAgainst: Int)
}