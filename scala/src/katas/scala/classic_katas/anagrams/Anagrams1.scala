package katas.scala.classic_katas.anagrams

import org.junit.Test
import org.scalatest.Matchers

import scala.io.Source

/**
 * User: dima
 * Date: 06/12/2012
 */

class Anagrams1 extends Matchers {
	@Test def shouldReadDictionary() {
		val words = readDictionary
		words.size should equal(234371)
	}

	@Test def shouldTransformWordIntoCharacterCountMap() {
		characterCountOf("") should equal(Map())
		characterCountOf("aaa") should equal(Map('a' -> 3))
		characterCountOf("abab") should equal(Map('a' -> 2, 'b' -> 2))
		characterCountOf("cat") should equal(Map('c' -> 1, 'a' -> 1, 't' -> 1))
	}

	@Test def shouldGroupWordsByCharacterCount() {
		groupByCharCount(Seq()) should equal(Map())
		groupByCharCount(Seq("abba")) should equal(Map(Map('a' -> 2, 'b' -> 2) -> Seq("abba")))
		groupByCharCount(Seq("abba", "abab")) should equal(Map(Map('a' -> 2, 'b' -> 2) -> Seq("abba", "abab")))
	}

	@Test def shouldFindAnagrams() {
		val words = readDictionary
		groupByCharCount(words)
	}

	private def groupByCharCount(words: Seq[String]): Map[Map[Char, Int], Seq[String]] = words match {
		case Seq() => Map().withDefault{map => Seq[String]()}
		case head :: tail =>
			val map = groupByCharCount(tail)
			val charCount = characterCountOf(head)
			map.updated(charCount, head +: map(charCount))
	}

	private def readDictionary: Seq[String] = {
		Source.fromFile("/usr/share/dict/words").getLines().toList.map { _.toLowerCase }.toSet.toSeq
	}

	private def characterCountOf(s: String): Map[Char, Int] = {
		s.toList.foldLeft(Map[Char, Int]().withDefault{char => 0}) { (map, char) =>
			map.updated(char, map(char) + 1)
		}
	}
}