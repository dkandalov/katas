package ru.db

import katas.java.db.StringUtil

/**
 * User: dima
 * Date: 05/01/2012
 */
class SU1 implements StringUtil {
  @Override
  String sort(String input) {
    input.chars.toList().sort().join("")
  }

  @Override
  String reverse(String input) {
    input.reverse()
  }

  @Override
  Map<Character, Integer> getDistribution(String input) {
    input.chars.toList().inject(new LinkedHashMap().withDefault { 0 }) { Map acc, char c ->
      acc.put(c, acc.get(c) + 1)
      acc // didn't return acc :(
    }
  }

  @Override
  String getFirstNSortedChars(String input, int topN) {
    sort(input)[0..<(topN > input.size() ? input.size() : topN)]
  }

  @Override
  String getUniqueCharsSortedByOccurrence(String input) {
    getDistribution(input).sort {-it.value}.keySet().join("")
  }

  @Override
  String getMode(String input) {
    def distribution = getDistribution(input)
    def maxFreq = distribution.max{it.value}?.value
    distribution.findResults {it.value == maxFreq ? it.key : null}.join("")
  }
}
