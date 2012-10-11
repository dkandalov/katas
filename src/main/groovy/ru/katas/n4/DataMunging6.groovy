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

    lines.each{println it}

    assert lines.size() == 30
  }
}
