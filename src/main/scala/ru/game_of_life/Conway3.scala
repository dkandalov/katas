package ru.game_of_life

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 23/10/2012
 */

class Conway3 extends ShouldMatchers {
	@Test def whenAllCellsAreDeadNothingHappens() {
		new Field(
			"""
			  |---
			  |---
			  |---
			""").next() should equal(new Field("""
        |---
        |---
        |---
      """))
	}

	@Test def lonelyCellDies() {
		new Field(
			"""
			  |---
			  |-0-
			  |---
			""").next() should equal(new Field("""
        |---
        |---
        |---
      """))
	}

	class Field(private val data: List[List[Char]]) {

		def this(s: String) {
			this(s.stripMargin.trim.split("\n").map{_.toList}.toList)
		}

		def next(): Field = {
			var newData: List[List[Char]] = List.fill(data.size){ List.fill(data.size){' '} }
//			for (row <- Range(0, data.size), col <- Range(0, data.size))
			new Field(newData)
		}

		override def toString = "\n" + data.map{_.mkString}.mkString("\n") + "\n"

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data
	}
}