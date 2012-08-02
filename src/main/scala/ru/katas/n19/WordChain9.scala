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

	@Test(timeout = 10000)
	def shouldFindWordChain_FromCatToDog_WithRealDictionary() {
		findMinWordChain("cat", "dog", loadDictionary()) should equal(Seq())
	}

	def findMinWordChain(fromWord: String, toWord: String, dictionary: Set[String]): Seq[String] = {
		if (fromWord.size != toWord.size) return Seq()
		val newDictionary = dictionary.filter(_.size == toWord.size)
		fromWord +: doFind(fromWord, toWord, newDictionary - fromWord, 1, Int.MaxValue)
	}

	def doFind(fromWord: String, toWord: String, dictionary: Set[String], depth: Int, minDepth: Int): Seq[String] = {
		if (fromWord == toWord) return Seq(toWord)
		if (depth >= minDepth) return Seq()

		var min = minDepth
		var result = Seq[String]()
		val nextWords = dictionary.filter { canMove(fromWord, _) }

		for (word <- nextWords) {
			val chain = doFind(word, toWord, dictionary - word, depth + 1, min)
			if (!chain.isEmpty && chain.size + depth < min) {
				min = chain.size + depth
				if (word != chain(0)) {
					result = word +: chain
				} else {
					result = chain
				}
				println(result.size + depth)
			}
		}
		result
	}

	def canMove(fromWord: String, toWord: String): Boolean = {
		if (fromWord.size != toWord.size) return false
		fromWord.diff(toWord).size == 1
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