package ru.db

import org.junit.Test

/**
 * User: dima
 * Date: 05/01/2012
 */
class SU1Test {
  def su = new SU1()

  @Test public void sort() {
    assert ["", "abc", "cba", "a1"].collect {su.sort(it)} == ["", "abc", "abc", "1a"]
  }

  @Test public void reverse() {
    assert ["", "abc", "cba", "a1"].collect {su.reverse(it)} == ["", "cba", "abc", "1a"]
  }

  @Test public void distribution() {
    assert su.getDistribution("") == [:]
    assert su.getDistribution("abc") == [a: 1, b: 1, c: 1].collectEntries {key, value -> [key.chars[0], value]}
    assert su.getDistribution("cbc") == [b: 1, c: 2].collectEntries {key, value -> [key.chars[0], value]}
    assert su.getDistribution("a1") == ["1": 1, a: 1].collectEntries {key, value -> [key.chars[0], value]}
  }

  @Test public void firstNSorted() {
    assert ["", "abc", "cba", "a1"].collect {su.getFirstNSortedChars(it, 2)} == ["", "ab", "ab", "1a"]
  }

  @Test public void uniqueCharsBuOccurrences() {
    assert ["", "abc", "cbc", "a1"].collect {su.getUniqueCharsSortedByOccurrence(it)} == ["", "abc", "cb", "a1"]
  }

  @Test public void mode() {
    assert ["", "abc", "cbc", "aa11"].collect {su.getMode(it)} == ["", "abc", "c", "a1"]
  }
}
