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
    text.each{println it}
  }
}
