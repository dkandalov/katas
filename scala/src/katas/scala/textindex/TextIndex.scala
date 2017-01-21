package katas.scala.textindex

import org.scalatest.Matchers
import org.junit.Test


class TextIndex extends Matchers {

	@Test def `text index`() {
		new Index("abc").positionOf("a") should equal(0)
		new Index("abc").positionOf("ab") should equal(0)
		new Index("abc").positionOf("abc") should equal(0)

		new Index("abbc").positionOf("a") should equal(0)
		new Index("abbc").positionOf("b") should equal(2)
		new Index("abbc").positionOf("bc") should equal(2)

		new Index("cba").positionOf("a") should equal(2)
		new Index("cba").positionOf("ba") should equal(1)
		new Index("cba").positionOf("cb") should equal(0)
		new Index("cba").positionOf("ab") should equal(-1)
	}

	@Test def `realistic example`() {
		val text = "Then we can use binary search on this table. For example, to find out whether " +
			"the phrase never mind appears in this text, we compare with long"

		new Index(text).positionOf("apple") should equal(-1)
		new Index(text).positionOf("never mind") should equal(89)
	}

	private class Index(text: String) {
		private val index = Range(0, text.size).sortBy{ i => text.substring(i) }

		def positionOf(s: String): Int = {
			var from = 0
			var to = index.size

			while (from < to) {
				val midIndex = (from + to) / 2
				val midValue = text.substring(index(midIndex))
				val result = startsWithCompare(midValue, s)

				if (result == 0) return index(midIndex)
				else if (result < 0) from = midIndex + 1
				else to = midIndex
			}
			-1
		}

		private def startsWithCompare(text: String, s: String): Int = {
			if (s.isEmpty) 0
			else if (text.isEmpty) 1 // implies that longer strings are alphabetically "greater"
			else {
				val result = text.head.compareTo(s.head)
				if (result == 0) startsWithCompare(text.tail, s.tail) else result
			}
		}
	}
}