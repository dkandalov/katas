package datamunging

import org.hamcrest.CoreMatchers.equalTo
import org.junit.Assert.assertThat
import org.junit.Test
import java.io.File
import kotlin.reflect.KClass

class DataMunging15 {
    private val weatherFile = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"
    private val footballFile = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"

    @Test fun `find day with minimum temperature spread`() {
        val days = File(weatherFile)
                .readLines()
                .map{ Day.parse(it) }
                .filter { it != null }
        assertThat(days.size, equalTo(30))

        val minDay = days.minBy{ it!!.max - it.min }
        assertThat(minDay, equalTo(Day("14", 61, 59)))
    }

    @Test fun `find team with minimum for-against goal difference`() {
        val teams = File(footballFile)
                .readLines()
                .map{ Team.parse(it) }
                .filter { it != null }
        assertThat(teams.size, equalTo(20))

        val team = teams.minBy{ Math.abs(it!!.goalsFor - it.goalsAgainst)}
        assertThat(team, equalTo(Team("Aston_Villa", 46, 47)))
    }

    private data class Day(val id: String, val max: Int, val min: Int) {
        companion object {
            fun parse(line: String): Day? {
                val list = line.trim().replace("*", "").split(Regex("\\s+"))
                if (list.size < 3) return null
                else return defaultToNullOn(NumberFormatException::class) {
                    Day(list[0], list[1].toInt(), list[2].toInt())
                }
            }
        }
    }
    private data class Team(val id: String, val goalsFor: Int, val goalsAgainst: Int) {
        companion object {
            fun parse(line: String): Team? {
                val parts = line.trim().split(Regex("\\s+"))
                if (parts.size < 9) return null
                else return defaultToNullOn(NumberFormatException::class) {
                    Team(parts[1], parts[6].toInt(), parts[8].toInt())
                }
            }
        }
    }

    companion object {
        fun <T> defaultToNullOn(aClass: KClass<out Exception>, f: () -> T): T? {
            try {
                return f()
            } catch(e: Exception) {
                if (aClass.java.isInstance(e)) return null
                else throw e
            }
        }
    }
}