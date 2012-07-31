package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 31/07/2012
 */

class WordChain9 extends ShouldMatchers {

	@Test def shouldFindWordChainsFrom_Cat_to_Dog() {
		findWordChain("cat", "dog", Set("cat")) should equal(Seq(Seq()))
	}

	def findWordChain(fromWord: String, toWord: String, dictionary: Set[String]) = {
		Seq()
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