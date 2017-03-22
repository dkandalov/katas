package ru.katas.n19

import org.junit.Ignore
import org.junit.Test

import static org.hamcrest.CoreMatchers.equalTo
import static org.junit.Assert.assertThat

class WordChain12 {
  @Test void findWordChainsInSimplestCases() {
    assert findWordChain("aaa", "aaa", ["aaa"]) == ["aaa"]
    assert findWordChain("aaa", "aab", ["aaa", "aab"]) == ["aaa", "aab"]
    assert findWordChain("aaa", "abc", ["aaa", "aba", "abc"]) == ["aaa", "aba", "abc"]
  }

  @Test void findShortestWordChain() {
    assert findWordChain("aaa", "abc", ["aaa", "aac", "baa", "bba", "bbc", "abc"]) == ["aaa", "aac", "abc"]
  }

  @Test void findShortestChainFromCatToDog() {
    def dictionary = loadDictionary()
    assert findWordChain("cat", "dog", dictionary) == ["cat", "cag", "dag", "dog"]
  }

  @Test void findShortestChainFromDogToCat() {
    def dictionary = loadDictionary()
    assert findWordChain("dog", "cat", dictionary) == ["dog", "dag", "cag", "cat"]
  }

  @Test void findShortestChainFromLeadToGold() {
    def dictionary = loadDictionary()
    assert findWordChain("lead", "gold", dictionary) == ["lead", "load", "goad", "gold"]
  }

  @Ignore
  @Test void findShortestChainFromJavaToRuby() {
    def dictionary = loadDictionary()
    assert findWordChain("java", "ruby", dictionary) == ["java", "load", "goad", "ruby"]
  }

  List findWordChain(String fromWord, String toWord, Collection dictionary) {
    if (!dictionary.contains(fromWord) || !dictionary.contains(toWord)) return []

    dictionary.removeAll{ it.length() != fromWord.length() }
    for (int maxDepth = 1; maxDepth < dictionary.size() + 1; maxDepth++) {
      def chain = doFindWordChain(fromWord, toWord, dictionary, 0, maxDepth)
      if (!chain.empty) return chain
    }
    []
  }

  List doFindWordChain(String fromWord, String toWord, Collection dictionary, int depth, int maxDepth) {
    if (depth >= maxDepth) return []
    if (fromWord == toWord) return [fromWord]

    def result = []
    def nextWords = dictionary.findAll{ isValidMove(fromWord, it) }
    def newDictionary = dictionary - fromWord

    for (String word : nextWords) {
      if (word == toWord) {
        println([fromWord] + [toWord])
        return [fromWord] + [toWord]

      } else {
        def chain = doFindWordChain(word, toWord, newDictionary, depth + 1, maxDepth)
        if (!chain.empty && chain.size() < maxDepth) {
          maxDepth = chain.size()
          result = [fromWord] + chain
        }
      }
    }
    result
  }

  @Test void validMove() {
    assert !isValidMove("", "")
    assert !isValidMove("aaa", "aaa")

    assert isValidMove("aaa", "aab")
    assert isValidMove("aaa", "aba")
    assert isValidMove("aaa", "baa")
    assert !isValidMove("aaa", "abb")
  }

  private static boolean isValidMove(fromWord, toWord) {
    if (fromWord.length() != toWord.length()) return false
    int diffs = 0
    for (int i = 0; i < fromWord.length(); i++) {
      if (fromWord[i] != toWord[i]) {
        diffs++
        if (diffs > 1) return false
      }
    }
    diffs == 1
  }

  @Test void shouldLoadDictionary() {
    def words = loadDictionary("/usr/share/dict/words")
    assertThat(words.size(), equalTo(234371))
  }

  static Set loadDictionary(String path = "/usr/share/dict/words") {
    new File(path).readLines().inject(new HashSet()) { set, word -> set.add(word.toLowerCase()); set }
  }
}