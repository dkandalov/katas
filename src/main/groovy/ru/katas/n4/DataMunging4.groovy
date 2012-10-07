package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 07/10/2012
 */
class DataMunging4 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    lines = lines.subList(8, lines.size() - 2).collect{ it.split()}
    lines.each {println it}
  }
}
