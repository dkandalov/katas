package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 01/10/2012
 */
class DataMunging3 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def text = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    assert 14 == text.subList(8, text.size() - 2).collect{ it.trim().split(/[\t\s]+/) }.
              collect{ [Integer.valueOf(it[0]), Integer.valueOf(it[1].replaceAll("\\*", "")), Integer.valueOf(it[2].replaceAll("\\*", ""))] }.
              min{ Math.abs(it[1] - it[2]) }[0]
  }

  @Test void shouldFindFootballTeam() {
    def text = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readLines()
    assert "Aston_Villa" == text.subList(5, text.size() - 1).collect{ it.trim().split(/[\t\s]+/) }.findAll{ it.size() >= 8 }.
            collect{ [it[1], Integer.valueOf(it[6]), Integer.valueOf(it[8])] }.
            min{ Math.abs(it[1] - it[2]) }[0]
  }
}
