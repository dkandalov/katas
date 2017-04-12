package katas.groovy.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 11/10/2012
 */
class DataMunging6 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    lines = filterLines(lines, 8, 2)
    def data = convertToData(lines, 0, 1, 2)

    def dayWithMinTempSpread = rowWithMinDiff(data).key

    lines.each{println it}

    assert lines.size() == 30
    assert data.size() == 30
    assert data[0] == [key: "1", value1: 88, value2: 59]
    assert dayWithMinTempSpread == "14"
  }

  @Test void shouldFindTeamWithMinGoalDifference() {
    def lines = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readLines()
    lines = filterLines(lines, 5, 1)
    def data = convertToData(lines, 1, 6, 8)

    def teamWithMinGoalDiff = rowWithMinDiff(data).key

    lines.each{ println it }

    assert lines.size() == 20
    assert data.size() == 20
    assert data[0] == [key: "Arsenal", value1: 79, value2: 36]
    assert teamWithMinGoalDiff == "Aston_Villa"
  }

  private static rowWithMinDiff(List data) {
    data.min { (it.value1 - it.value2).abs() }
  }

  private static convertToData(List<String> lines, int keyIndex, int value1Index, int value2Index) {
    lines.collect{ it.split() }.collect{ [key: it[keyIndex], value1: toInt(it[value1Index]), value2: toInt(it[value2Index])] }
  }

  private static List<String> filterLines(List<String> lines, int headDrop, int tailDrop) {
    lines.subList(headDrop, lines.size() - tailDrop).findAll{ !it.trim().matches(/--+/) }
  }

  private static int toInt(String s) {
    Integer.parseInt(s.replace("*", ""))
  }
}
