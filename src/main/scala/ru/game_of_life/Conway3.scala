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

	@Test def cellWithEnoughNeighboursBecomesAlive() {
		new Field(
			"""
			  |0-0
			  |---
			  |---
			""").next() should equal(new Field("""
				|---
				|-0-
				|-0-
			"""))
	}

	@Test def gettingCellShouldWrapAroundFieldBorder() {
		val field = new Field(
			"""
			  |--0
			  |-0-
			  |0--
			""")
		field.cellAt(0, 0) should equal('-')
		field.cellAt(0, 1) should equal('-')
		field.cellAt(0, 2) should equal('0')
		field.cellAt(-1, 0) should equal('0')
		field.cellAt(-2, 0) should equal('-')
		field.cellAt(-3, 0) should equal('-')

		field.cellAt(-1, -1) should equal('-')
		field.cellAt(-2, -2) should equal('0')
		field.cellAt(0, 5) should equal('0')
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
					else if (hasEnoughNeighbours(row, col)) '0'
					else data(row)(col)
				newData = newData.updated(row, newData(row).updated(col, newCellState))
			}
			new Field(newData)
		}

		def cellAt(row: Int, col: Int): Char = {
			def wrap(n: Int) = (n + data.size) % data.size
			data(wrap(row))(wrap(col))
		}

		private def isLonelyCell(row: Int, col: Int): Boolean = {
			cellsAround(row, col).count{_ == '0'} < 2
		}

		private def hasEnoughNeighbours(row: Int, col: Int): Boolean = {
			val count = cellsAround(row, col).count{_ == '0'}
			count >= 2 && count <= 3
		}

		private def cellsAround(row: Int, col: Int): Seq[Char] = {
			List((-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1)).map{ point => cellAt(point._1, point._2) }
		}

		override def toString = "\n" + data.map{_.mkString}.mkString("\n") + "\n"

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data
	}
}