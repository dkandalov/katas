package ru.katas.n4

import org.junit.Test

/**
 * User: dima
 * Date: 01/10/2012
 */
class DataMunging3 {
  private static String BASE_PATH = "${System.getenv("HOME")}/IdeaProjects/katas/src/main/scala/ru/katas/n4"

  @Test void shouldFindDayWithMinTemperatureSpread() {
    def text = new File("${BASE_PATH}/weather.dat").readLines()

    def dayWithMinTemperatureSpread = process(text, 8, 2).collect { extractDataFrom(it, 0, 1, 2) }.min { diff(it) }.id
    assert "14" == dayWithMinTemperatureSpread
  }

  @Test void shouldFindFootballTeam() {
    def text = new File("${BASE_PATH}/football.dat").readLines()

    def teamWithMinGoalDiff = process(text, 5, 1).collect { extractDataFrom(it, 1, 6, 8) }.min { diff(it) }.id
    assert "Aston_Villa" == teamWithMinGoalDiff
  }

  private static diff(data) {
    (data.value1 - data.value2).abs()
  }

  private static extractDataFrom(String[] it, int index1, int index2, int index3) {
    [
            id: it[index1],
            value1: parseInt(it[index2]),
            value2: parseInt(it[index3])
    ]
  }

  public static int parseInt(String s) {
    s.replaceAll("\\*", "").toInteger()
  }

  private static ArrayList<String[]> process(List<String> text, int startSkip, int endSkip) {
    text.subList(startSkip, text.size() - endSkip).collect { it.trim().split(/[\t\s]+/) }.findAll { it.size() >= 8 }
  }
}
