package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 31/07/2012
 */

class WordChain9 extends ShouldMatchers {

	def canMove(fromWord: String, toWord: String): Boolean = {
		if (fromWord == toWord) return false
		if (fromWord.size != toWord.size) return false
		true
	}

	@Test def shouldDetermineCorrectTransition() {
		canMove("", "") should be === (true)
		canMove("a", "") should be === (false)
		canMove("a", "a") should be === (false)
	}

	@Test def shouldLoadStandardMacDictionary() {
		val words = loadDictionary()
		words.size should equal(234371)
	}

	def loadDictionary(): Set[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().map{_.toLowerCase}.toSet
	}
}