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
		val words = readWords()
		awords.size should equal(235886)
	}

	def readWords(): Seq[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().toSeq
	}
}