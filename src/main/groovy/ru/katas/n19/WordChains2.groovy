package ru.katas.n19

import org.junit.Test
import ru.util.Mess
import ru.util.Pomodoro

/**
 * User: dima
 * Date: 03/05/2012
 */
@Pomodoro("2")
@Mess
@SuppressWarnings("GroovyMissingReturnStatement")
class WordChains2 {
  @Test public void aaa() {
    def words = new File("/usr/share/dict/words").readLines()
    findShortestWordChain("cat", "dog", words).with {
      println(it)
      assert it == ["cat", "cag", "cog", "dog"]
    }
  }

  def minChainCache = [:].withDefault {[:]} // caching word chains is way better than caching word to word transform

  def findShortestWordChain(String fromWord, String toWord, List<String> words) {
    if (fromWord.length() != toWord.length()) return []
    words = words.findAll { it.length() == fromWord.length() }

    def minChain = []
    def minChainSize = Integer.MAX_VALUE

    def f = null
    f = { from, to, chain ->
      def result = minChainCache[fromWord][toWord]
      if (result != null) return result

      if (from == to) {
        minChain = chain + to
        minChainSize = minChain.size()
        println "done: ${minChainSize} : ${minChain}"
        return
      }

      if (chain.size() >= minChainSize - 2) return // was off-by-one; this slowed down everything
      chain += from

      def nextWords = (words - chain).findAll { word -> canBeNext(from, word) }
      nextWords.each { nextWord ->
        if (chain.size() < minChainSize)
          f(nextWord, to, chain)
      }
    }

    f(fromWord, toWord, [])
    minChainCache[fromWord][toWord] = minChain
    minChain
  }

  static boolean canBeNext(String fromWord, String toWord) {
    int diffs = 0
    while (!fromWord.empty && !toWord.empty) {
      if (fromWord[0] != toWord[0]) diffs++
      if (diffs > 1) return false

      fromWord = fromWord.drop(1)
      toWord = toWord.drop(1)
    }
    return true
  }
}
