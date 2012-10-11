package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 11/10/2012
 */
class DataMunging6 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    lines = lines.subList(8, lines.size() - 2)
    def data = lines.collect{ it.split() }.collect{ [key: it[0], value1: it[1], value2: it[2]] }

    lines.each{println it}

    assert lines.size() == 30
    assert data.size() == 30
    assert data[0].key == "1"
    assert data[0].value1 == "1"
    assert data[0].value2 == "1"
  }
}
