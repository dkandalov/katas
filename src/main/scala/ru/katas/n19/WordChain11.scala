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
		findShortestWordChain("aaa", "ccc", Seq("aaa", "ccc")) should equal(Seq())
		findShortestWordChain("aaa", "ccc", Seq("aaa", "aac", "acc", "ccc")) should equal(Seq("aaa", "aac", "acc", "ccc"))
	}

	def findShortestWordChain(fromWord: String, toWord: String, dict: Seq[String]): Seq[String] = {
		if (fromWord == toWord) return Seq(toWord)
		Seq()
	}

	@Test def shouldFindNextWords() {

	}
}