package ru.katas.n19

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source

/**
 * User: dima
 * Date: 10/08/2012
 */

class WordChain11 extends ShouldMatchers {

	@Test def shouldFindSimpleWordChains() {
		findShortestWordChain("aaa", "aaa", Seq("aaa")) should equal(Seq("aaa"))
		findShortestWordChain("aaa", "ccc", Seq("aaa", "ccc")) should equal(Seq())
		findShortestWordChain("aaa", "ccc", Seq("aaa", "aac", "acc", "ccc")) should equal(Seq("aaa", "aac", "acc", "ccc"))
	}

	@Test def shouldFindShortestWordChains() {
		findShortestWordChain("aaa", "ccc", Seq("aaa", "aac", "acc", "abc", "cbc", "ccc")) should equal(Seq("aaa", "aac", "acc", "ccc"))
	}

	@Test def shouldFindShortestWordChain_From_Cat_to_Dog() {
		val dict = loadMacDict()
		findShortestWordChain("cat", "dog", dict) should equal(Seq("cat", "dog"))
	}

	private def loadMacDict(): Seq[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().map(_.toLowerCase).toSeq
	}

	def findShortestWordChain(fromWord: String, toWord: String, dict: Seq[String]): Seq[String] = {
		val moves = findAllCorrectMoves(dict)
		for (maxDepth <- 2 to dict.size + 1) {
			val chain = doFind(fromWord, toWord, dict, 1, maxDepth, moves)
			if (!chain.isEmpty) return chain
		}
		Seq()
	}

	type Moves = Map[String, Seq[String]]

	private def findAllCorrectMoves(dict: Seq[String]): Moves = {
		Map()
	}

	private def doFind(fromWord: String, toWord: String, dict: Seq[String], depth: Int, maxDepth: Int, moves: Moves): Seq[String] = {
		if (depth >= maxDepth) return Seq()
		if (fromWord == toWord) return Seq(toWord)

		val nextWords = dict.filter{ canBeNext(fromWord, _) }
		var newMaxDepth = maxDepth
		var result = Seq[String]()

		for (word <- nextWords) {
			val chain = doFind(word, toWord, dict.filterNot(nextWords.contains(_)), depth + 1, newMaxDepth, moves)
			if (!chain.isEmpty) {
				result = fromWord +: chain
				newMaxDepth = depth + chain.size
			}
		}

		result
	}

	@Test def shouldFindNextWords() {
		canBeNext("aaa", "aaa") should be(false)
		canBeNext("aaa", "aab") should be(true)
		canBeNext("aaa", "aba") should be(true)
		canBeNext("aaa", "baa") should be(true)
		canBeNext("aaa", "bba") should be(false)
	}

	private def canBeNext(fromWord: String, toWord: String): Boolean = {
		fromWord.diff(toWord).size == 1
	}
}