package katas.scala.classic_katas.wordchain

import org.junit.Test
import org.scalatest.Matchers

import scala.io.Source

/**
 * User: dima
 * Date: 10/08/2012
 */

class WordChain11 extends Matchers {

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
		findShortestWordChain("cat", "dog", dict) should equal(Seq("cat", "cag", "cog", "dog"))
		findShortestWordChain("dog", "cat", dict) should equal(Seq("dog", "cog", "cag", "cat"))
	}

	private def loadMacDict(): Seq[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().map(_.toLowerCase).toSeq
	}

	def findShortestWordChain(fromWord: String, toWord: String, dict: Seq[String]): Seq[String] = {
		val newDict = dict.filter(_.length == toWord.length)
		val moves = findAllCorrectMoves(newDict)
		for (maxDepth <- 2 to dict.size + 1) {
			val chain = doFind(fromWord, toWord, newDict, 1, maxDepth, moves)
			if (!chain.isEmpty) return chain
		}
		Seq()
	}

	type Moves = Map[String, Seq[String]]

	private def findAllCorrectMoves(dict: Seq[String]): Moves = {
		dict.foldLeft(Map[String, Seq[String]]()) { (acc, word) =>
			acc.updated(word, dict.filter(canBeNext(word, _)))
		}
	}

	private def doFind(fromWord: String, toWord: String, dict: Seq[String], depth: Int, maxDepth: Int, moves: Moves): Seq[String] = {
		if (depth >= maxDepth) return Seq()
		if (fromWord == toWord) return Seq(toWord)

		val nextWords = moves(fromWord)
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
		canBeNext("aaa", "aaa") should equal(false)
		canBeNext("aaa", "aab") should equal(true)
		canBeNext("aaa", "aba") should equal(true)
		canBeNext("aaa", "baa") should equal(true)
		canBeNext("aaa", "bba") should equal(false)
	}

	private def canBeNext(fromWord: String, toWord: String, diffs: Int = 0): Boolean = {
		if (diffs > 1) return false
		else if (fromWord.isEmpty) diffs == 1
		else if (fromWord(0) == toWord(0))
			canBeNext(fromWord.tail, toWord.tail, diffs)
		else
			canBeNext(fromWord.tail, toWord.tail, diffs + 1)
	}
}