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
			  |----
			  |-00-
			  |----
			  |----
			""").next() should equal(new Field("""
				|-00-
				|----
				|-00-
				|----
			"""))
	}

	@Test def overPopulatedCellDies() {
		new Field(
			"""
			  |-0-
			  |000
			  |-0-
			""").next().cellAt(1, 1).toString should equal("-")
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

	case class Cell(c: Char)
	case object LivingCell extends Cell('0') {
		override def toString = c.toString
	}
	case object DeadCell extends Cell('-') {
		override def toString = c.toString
	}
	case object UndefinedCell extends Cell(' ') {
		override def toString = c.toString
	}

	class Field(private val data: List[List[Char]]) {

		def this(s: String) {
			this(s.stripMargin.trim.split("\n").map{_.toList}.toList)
		}

		def next(): Field = {
			var newData: List[List[Char]] = List.fill(data.size){ List.fill(data.size){' '} }
			for (row <- 0 until data.size; col <- 0 until data.size) {
				val liveCellsAround = cellsAround(row, col).count{_ == '0'}
				val newCellState =
					if (liveCellsAround < 2 || liveCellsAround > 3) '-'
					else '0'
				newData = newData.updated(row, newData(row).updated(col, newCellState))
			}
			new Field(newData)
		}

		def cellAt(row: Int, col: Int): Char = {
			def wrap(n: Int) = (n + data.size) % data.size
			data(wrap(row))(wrap(col))
		}

		private def cellsAround(row: Int, col: Int): Seq[Char] = {
			List((-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1)).map{ point =>
				cellAt(row + point._1, col + point._2)
			}
		}

		override def toString = "\n" + data.map{_.mkString}.mkString("\n") + "\n"

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data
	}
}