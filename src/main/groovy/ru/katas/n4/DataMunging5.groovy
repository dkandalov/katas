package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 09/10/2012
 */
class DataMunging5 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def text = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    text = text.subList(8, text.size() - 2).collect{ it.split() }
            .collect{ [key: it[0], value1: toInt(it[1]), value2: it[2]] }

    text.each{println it}
    assert text.size() == 30
    assert text[0].key == "1"
    assert text[0].value1 == 88
    assert text[0].value2 == 59
  }

  private static def toInt(String s) {
    s
  }
}
