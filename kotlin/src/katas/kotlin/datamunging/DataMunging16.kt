package katas.kotlin.datamunging

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test
import java.io.File

class DataMungingTest16 {
    @Test fun `weather`() {
        val key = File("src/katas/kotlin/datamunging/weather.dat").readLines()
            .map{ parse(it, 0, 1, 2) }
            .filterNotNull()
            .minByOrNull { it: Entry -> Math.abs(it.max - it.min) }!!.key
        assertThat(key, equalTo<Any>("14"))
    }
    
    @Test fun `football`() {
        val key = File("src/katas/kotlin/datamunging/football.dat").readLines()
            .map{ parse(it, 1, 6, 8) }
            .filterNotNull()
            .minByOrNull { it: Entry -> Math.abs(it.max - it.min) }!!.key
        assertThat(key, equalTo<Any>("Aston_Villa"))
    }

    fun parse(s: String, i: Int, i1: Int, i2: Int): Entry? {
        return try {
            s.trim().split(Regex("\\s+")).let {
                Entry(it[i], it[i1].toInt(), it[i2].toInt())
            }
        } catch(ignored: Exception) {
            null
        }
    }

    data class Entry(val key: String, val min: Int, val max: Int)
}