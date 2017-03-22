package ru.db

import katas.java.db.StringUtil

import static java.lang.Math.min

/**
 * User: dima
 * Date: 07/01/2012
 */
class SU2 implements StringUtil {
  @Override
  String sort(String input) {
    input.toList().sort().join("")
  }

  @Override
  String reverse(String input) {
    input.toList().reverse().join("")
  }

  @Override
  Map<Character, Integer> getDistribution(String input) {
    input.chars.toList().inject(new LinkedHashMap().withDefault {0}) { Map map, char c ->
      map[c] = map[c] + 1 // put one-character strings instead of chars (using input.chars.toList() seems to be cleaner than input.toList() and then converting one-char strings to chars)
      map
    }
  }

  @Override
  String getFirstNSortedChars(String input, int topN) {
    sort(input)[0..<min(topN, input.length())]
  }

  @Override
  String getUniqueCharsSortedByOccurrence(String input) {
    getDistribution(input).sort{-it.value}.collect{it.key}.join("") // sorted by "+it.value"; didn't join()
  }

  @Override
  String getMode(String input) {
    if (input.empty) return ""
    // sorted by "+it.value"; didn't join characters properly
    getDistribution(input).groupBy{-it.value}.sort{it.key}.values().toList()[0].keySet().join("")
  }
}
