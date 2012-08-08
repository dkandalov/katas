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

    def result = []
    for (String word in nextWords) {
      def chain = doFind(word, toWord, dict - word, minDepth)
      if (!chain.empty) {
        result = [fromWord] + chain
      }
    }
    result
  }

  private static boolean canMove(String fromWord, String toWord) {
    int diffs = 0
    def list = toWord.toList()
    for (c in fromWord) {
      if (!list.remove(c)) {
        diffs++
        if (diffs > 1) return false
      }
    }
    diffs == 1
  }

  @Test void shouldDetermineIfMoveIsValid() {
    assert !canMove("a", "a")
    assert canMove("a", "b")

    assert canMove("aaa", "aab")
    assert canMove("aaa", "aba")
    assert canMove("aaa", "baa")
    assert !canMove("aaa", "abb")
  }
}