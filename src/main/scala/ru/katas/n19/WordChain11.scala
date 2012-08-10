package ru.katas.n19

import org.junit.Test
import org.scalatest.matchers.ShouldMatchers

/**
 * User: dima
 * Date: 10/08/2012
 */

class WordChain11 extends ShouldMatchers {

	@Test def shouldFindSimpleWordChains() {
		findShortestWordChain("aaa", "aaa", Seq("aaa")) should equal(Seq("aaa"))
	}

	def findShortestWordChain(fromWord: String, toWord: String, dict: Seq[String]): Seq[String] = {
		Seq()
	}
}