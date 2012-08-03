package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source

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

	@Test//(timeout = 10000)
	def shouldFindWordChain_FromCatToDog_WithRealDictionary() {
		findMinWordChain("cat", "dog", loadDictionary()) should equal(Seq())
	}

	def findMinWordChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		if (fromWord.size != toWord.size) return Seq()
		val newDictionary = dictionary.filter(_.size == toWord.size)
		doFind(fromWord, toWord, newDictionary - fromWord, 1, Int.MaxValue)
	}

	private def doFind(fromWord: String, toWord: String, dictionary: Set[String], depth: Int, minDepth: Int): Seq[String] = {
		if (depth >= minDepth) return Seq()
		if (fromWord == toWord) {
			println(depth)
			return Seq(toWord)
		}

		var min = minDepth
		var result = Seq[String]()
		var newDict = dictionary

		for (word <- dictionary.filter{ canMove(fromWord, _) }) {
			newDict -= word
			val chain = doFind(word, toWord, newDict, depth + 1, min)
			if (!chain.isEmpty) {
				min = chain.size + depth
				result = fromWord +: chain
			}
		}
		result
	}

	def canMove(fromWord: String, toWord: String): Boolean = {
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
		true
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