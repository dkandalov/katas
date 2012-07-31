package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.io.Source

/**
 * User: dima
 * Date: 31/07/2012
 */

class WordChain9 extends ShouldMatchers {

	@Test def aaa() {
		val words = loadDictionary()
		words.size should equal(235886)
	}

	def loadDictionary(): Set[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().map{_.toLowerCase}.toSet
	}
}