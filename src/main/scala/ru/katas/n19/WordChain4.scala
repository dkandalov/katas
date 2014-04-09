package ru.katas.n19

import org.scalatest.Matchers
import org.junit.Test
import scala.io.Source
import ru.util.{Fail, Arcade}

/**
 * User: dima
 * Date: 14/05/2012
 */

@Arcade @Fail
class WordChain4 extends Matchers {
	@Test def aaa() {
		val dictionary = Source.fromFile("/usr/share/dict/words").getLines().toSeq
		findMinChain("cat", "cat", dictionary) should equal(Seq("cat"))
		findMinChain("cat", "cot", dictionary) should equal(Seq("cat", "cot"))
//		findMinChain("cat", "dog", dictionary) should equal(Seq("cat", "dog"))
	}

	var minSize = Int.MaxValue

	def findMinChain(fromWord: String, toWord: String, dictionary: Seq[String]): Seq[String] = {
		if (fromWord.length() != toWord.length()) return Seq()
		if (fromWord == toWord) return Seq(fromWord)

		val words = dictionary.filter(_.length() == fromWord.length()).diff(Seq(fromWord))
		val minChain = doFindMinChain(fromWord, toWord, words)
		if (minChain.isEmpty) Seq() else fromWord +: minChain
	}

	def doFindMinChain(fromWord: String, toWord: String, dictionary: Seq[String]): Seq[String] = {
		if (dictionary.isEmpty) return Seq()
		if (fromWord == toWord) Seq(toWord)

		val result = Seq()
		dictionary.foreach { word =>
			if (canBeNext(word, toWord)) {
				val minChain = doFindMinChain(word, toWord, dictionary.diff(Seq(word)))
				if (minChain.isEmpty) fromWord +: minChain else Seq()
			}
		}
		result
	}

	def canBeNext(word: String, toWord: String): Boolean = {
		word.diff(toWord).size == 1
	}
}