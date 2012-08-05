package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source
import collection.mutable

/**
 * User: dima
 * Date: 31/07/2012
 */

class WordChain9 extends ShouldMatchers {

	@Test def shouldFindSingleWordChain() {
		findMinWordChain("aaa", "bbb", Set("aaa", "aab", "abb", "bbb")) should equal(Seq("aaa", "aab", "abb", "bbb"))
	}

	@Test def shouldChooseShortestWordChain() {
		findMinWordChain("aaa", "ccc", Set("aaa", "ccc", "caa", "aca", "aac", "acc", "cca")) should equal(Seq("aaa", "aac", "acc", "ccc"))
	}

	@Test(timeout = 20000)
	def shouldFindWordChain_FromCatToDog_WithRealDictionary() {
		findMinWordChain("cat", "dog", loadDictionary()) should equal(Seq("cat", "cag", "cog", "dog"))
	}

	@Test
	def shouldFindWordChain_FromLeadToGold_WithRealDictionary() {
		findMinWordChain("lead", "gold", loadDictionary()) should equal(Seq("lead", "gold"))
	}

	def findMinWordChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		if (fromWord.size != toWord.size) return Seq()
		var filteredDict = dictionary.filter(_.size == toWord.size)
		filteredDict.foreach { word =>
			wordConnections(word) = findAllConnectionsOf(word, filteredDict)
		}
		filteredDict = filteredDict.filter(wordConnections(_).size > 0)

		doFind(fromWord, toWord, filteredDict, 1, filteredDict.size + 1)
	}

	val wordConnections: mutable.Map[String, Set[String]] = mutable.Map()

	private def doFind(fromWord: String, toWord: String, dictionary: Set[String], depth: Int, maxDepth: Int): Seq[String] = {
		if (depth >= maxDepth) return Seq()
		if (fromWord == toWord) {
			println(depth)
			return Seq(toWord)
		}

		var min = maxDepth
		var result = Seq[String]()
		val nextWords = wordConnections(fromWord).toSeq.sortBy(wordConnections(_).size)
		val newDict = (dictionary -- nextWords) - fromWord + toWord

		for (word <- nextWords) {
			val chain = doFind(word, toWord, newDict, depth + 1, min)
			if (!chain.isEmpty) {
				min = chain.size + depth
				result = fromWord +: chain
			}
		}
		result
	}

	private def findAllConnectionsOf(word: String, dict: Set[String]): Set[String] = {
		(dict - word).foldLeft(Set[String]()) { (acc, toWord) =>
			if (canMove(word, toWord)) acc + toWord else acc
		}
	}

	private def canMove(fromWord: String, toWord: String): Boolean = {
		if (fromWord.size == 0 || toWord.size == 0) return false
		if (fromWord.size != toWord.size) return false
		var i = 0
		var errCount = 0
		while (i < fromWord.size) {
			if (fromWord(i) != toWord(i)) {
				errCount += 1
				if (errCount > 1) return false
			}
			i += 1
		}
		errCount == 1
	}

	@Test def shouldDetermineCorrectTransition() {
		assert(!canMove("", ""))
		assert(!canMove("a", ""))
		assert(!canMove("a", "a"))

		assert(canMove("a", "b"))
		assert(canMove("aa", "ab"))
		assert(!canMove("aa", "bb"))

		assert(canMove("aaa", "aab"))
		assert(canMove("aaa", "aba"))
		assert(canMove("aaa", "baa"))
		assert(!canMove("aaa", "bba"))

		assert(canMove("cat", "cot"))
		assert(!canMove("cat", "dog"))
	}

	@Test def shouldLoadStandardMacDictionary() {
		val words = loadDictionary()
		words.size should equal(234371)
	}

	def loadDictionary(): Set[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().map{_.toLowerCase}.toSet
	}
}