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

  def findShortestChain(String fromWord, String toWord, Collection dict) {
    doFind(fromWord, toWord, dict - fromWord, dict.size())
  }

  private def doFind(String fromWord, String toWord, Collection dict, int minDepth) {
    if (fromWord == toWord) return [toWord]
    def nextWords = dict.findAll() { String word -> canMove(fromWord, word) }
    []
  }

  private static boolean canMove(String fromWord, String toWord) {
    false
  }
}