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
			for (row <- 0 until data.size; col <- 0 until data.size) {
				val newCellState =
					if (isLonelyCell(row, col)) '-'
					else data(row)(col)
				newData = newData.updated(row, newData(row).updated(col, newCellState))
			}
			new Field(newData)
		}

		private def isLonelyCell(row: Int, col: Int): Boolean = {
			cellsAround(row, col).count{ _ == '0' } < 2
		}

		private def cellsAround(row: Int, col: Int): List[Char] = {
			List()
		}

		override def toString = "\n" + data.map{_.mkString}.mkString("\n") + "\n"

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data
	}
}