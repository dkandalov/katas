package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 11/10/2012
 */
class DataMunging6 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    lines = filterLines(lines, 8, 2)
    def data = lines.collect{ it.split() }.collect{ [key: it[0], value1: toInt(it[1]), value2: toInt(it[2])] }

    def dayWithMinTempSpread = data.min { (it.value1 - it.value2).abs() }.key

    lines.each{println it}

    assert lines.size() == 30
    assert data.size() == 30
    assert data[0].key == "1"
    assert data[0].value1 == 88
    assert data[0].value2 == 59
    assert dayWithMinTempSpread == "14"
  }

  @Test void shouldFindTeamWithMinGoalDifference() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readLines()
    lines = filterLines(lines, 5, 1)
    def data = convertToData(lines)

    def teamWithMinGoalDiff = data.min{ (it.value1 - it.value2).abs() }.key

    lines.each{ println it }

    assert lines.size() == 20
    assert data.size() == 20
    assert data[0].key == "Arsenal"
    assert data[0].value1 == 79
    assert data[0].value2 == 36
    assert teamWithMinGoalDiff == "Aston_Villa"
  }

  private static convertToData(List<String> lines) {
    lines.collect{ it.split() }.collect{ [key: it[1], value1: toInt(it[6]), value2: toInt(it[8])] }
  }

  private static List<String> filterLines(List<String> lines, int headDrop, int tailDrop) {
    lines.subList(headDrop, lines.size() - tailDrop).findAll{ !it.trim().matches(/--+/) }
  }

  private static int toInt(String s) {
    Integer.parseInt(s.replace("*", ""))
  }
}
