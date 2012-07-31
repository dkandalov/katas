package ru.katas.n19

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 31/07/2012
 */

class WordChain9 extends ShouldMatchers {

	@Test def aaa() {
		val words = readWords()
		words.size should equal(10000)
	}
	def readWords(): Seq[String] = {
		Seq()

	}
}