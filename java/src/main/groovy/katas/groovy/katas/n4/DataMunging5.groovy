package katas.groovy.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 09/10/2012
 */
class DataMunging5 {
  @Test void shouldFindDayWithMinTemperatureSpread() {
    def text = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readLines()
    text = preProcess(text, 8, 2)
    def data = convertToData(text, 0, 1, 2)

    def dayWithMinTemperatureSpread = data.min { (it.value1 - it.value2).abs() }.key

    assert text.size() == 30
    assert data.size() == 30
    assert data[0].key == "1"
    assert data[0].value1 == 88
    assert data[0].value2 == 59

    assert dayWithMinTemperatureSpread == "14"
  }

  @Test public void findTeamWithMinGoalDifference() {
    def text = new File("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readLines()
    text = preProcess(text, 5, 1)
    def data = convertToData(text, 1, 6, 8)

    def teamWithMinGoalDiff = data.min{ (it.value1 - it.value2).abs() }.key

    assert text.size() == 20
    assert data.size() == 20
    assert data[0].key == "Arsenal"
    assert data[0].value1 == 79
    assert data[0].value2 == 36

    assert teamWithMinGoalDiff == "Aston_Villa"
  }

  private static def convertToData(List<String> text, int keyIndex, int value1Index, int value2Index) {
    text.collect{ it.split() }.collect{ [key: it[keyIndex], value1: asInt(it[value1Index]), value2: asInt(it[value2Index])] }
  }

  private static List<String> preProcess(List<String> text, int headLinesToSkip, int tailLinesToSkip) {
    text.subList(headLinesToSkip, text.size() - tailLinesToSkip).findAll{ !it.trim().matches(/---+/) }
  }

  private static def asInt(String s) {
    Integer.valueOf(s.replace("*", ""))
  }
}
