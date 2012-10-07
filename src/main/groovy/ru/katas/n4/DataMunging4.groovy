package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 07/10/2012
 */
class DataMunging4 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    lines = extractDataFrom(lines, 8, 2)
            .collect{ it.split() }.collect { [key:it[0], value1: asInt(it[1]), value2: asInt(it[2])] }

    def dayWithMinSpread = lines.min{ (it.value1 - it.value2).abs() }.key

    assert lines.size() == 30
    assert dayWithMinSpread == "14"
  }

  @Test public void shouldFindTeamWithMinGoalDifference() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readLines()
    lines = extractDataFrom(lines, 5, 1)
            .collect{ it.split() }.collect { [key: it[1], value1: asInt(it[6]), value2: asInt(it[8])] }

    def teamWithMinGoalDiff = lines.min{ (it.value1 - it.value2).abs() }.key

    assert lines.size() == 20
    assert teamWithMinGoalDiff == "Aston_Villa"
  }

  private static List<String> extractDataFrom(List<String> lines, int skipAtHead, int skipAtTail) {
    lines.subList(skipAtHead, lines.size() - skipAtTail).findAll{!it.trim().matches(/--+/)}
  }

  private static convertToValues(List<String> lines, int keyIndex, int value1Index) {

  }

  private static def asInt(String s) {
    s.replaceAll(/\*/, "").toInteger()
  }
}
