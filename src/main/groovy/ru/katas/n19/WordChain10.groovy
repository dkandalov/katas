package ru.katas.n19

import org.junit.Test

/**
 * User: dima
 * Date: 08/08/2012
 */
class WordChain10 {
  @Test void shouldFindSimpleWordChains() {
    assert findShortestChain("aaa", "bbb", []) == []
    assert findShortestChain("aaa", "aaa", []) == ["aaa"]
    assert findShortestChain("aaa", "bbb", ["aaa", "aab", "abb", "bbb"]) == ["aaa", "aab", "abb", "bbb"]
  }

  private def findShortestChain(String fromWord, String toWord, Collection dict) {
    if (fromWord == toWord) return [toWord]
    []
  }
}
