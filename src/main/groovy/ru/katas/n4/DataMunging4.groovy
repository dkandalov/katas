package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 07/10/2012
 */
class DataMunging4 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    lines = lines.subList(8, lines.size() - 2).collect{ it.split() }
            .collect{ [key:it[0], value1: asInt(it[1]), value2: asInt(it[2])] }

    def dayWithMinSpread = lines.min{ (it.value1 - it.value2).abs() }.key

    assert lines.size() == 30
    assert dayWithMinSpread == "14"
  }

  private static def asInt(String s) {
    s.replaceAll(/\*/, "").toInteger()
  }
}
