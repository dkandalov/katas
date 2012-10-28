package ru.game_of_life

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 27/10/2012
 */

class Conway4 extends ShouldMatchers {

	@Test def fieldShouldReturnCellState() {
		def field = new Field(
			"""
			  |0--
			  |-0-
			  |--0
			""")
		field.cellAt(0, 0) should equal('0')
		field.cellAt(0, 1) should equal('-')
		field.cellAt(0, 2) should equal('-')

		field.cellAt(1, 0) should equal('-')
		field.cellAt(1, 1) should equal('0')
		field.cellAt(1, 2) should equal('-')
	}

	@Test def fieldBorderShouldWrap() {
		def field = new Field(
			"""
			  |0--
			  |-0-
			  |--0
			""")

		field.cellAt(-1, -1) should equal('0')

		field.cellAt(-1, 0) should equal('-')
		field.cellAt(-2, 0) should equal('-')
		field.cellAt(-3, 0) should equal('0')

		field.cellAt(0, -1) should equal('-')
		field.cellAt(0, -2) should equal('-')
		field.cellAt(0, -3) should equal('0')
	}

	@Test def whenFieldIsEmptyNothingHappens() {
		new Field(
			"""
			  |---
			  |---
			  |---
			""").next() should equal(new Field(
			"""
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
			""").next() should equal(new Field(
			"""
			  |---
			  |---
			  |---
			"""))
	}

	@Test def withEnoughNeighboursCellBecomesAlive() {
		new Field(
			"""
			  |0-0
			  |---
			  |0--
			""").next().cellAt(1, 1) should equal('0')
	}

	class Field(val data: List[List[Char]]) {

		def this(s : String) {
			this(s.trim.stripMargin.split("\n").toList.map{ _.toList })
		}

		def cellAt(row: Int, col: Int): Char = {
			def wrap = { n: Int => (n + data.size) % data.size}
			data(wrap(row))(wrap(col))
		}

		def next(): Field = {
			var newData = List.fill(data.size, data.size){ ' ' }
			for (row <- 0 until data.size; col <- 0 until data.size) {
				val cellState =
					if (isLonelyCell(row, col)) '-'
					else data(row)(col)
				newData = newData.updated(row, newData(row).updated(col, cellState))
			}
			new Field(newData)
		}

		private def isLonelyCell(row: Int, col: Int) = {
			cellsAround(row, col).count{ _ == '0' } < 2
		}

		private def cellsAround(row: Int, col: Int): Seq[Char] = {
			Seq((-1, 0), (0, 1), (0, 1), (0, -1), (-1, -1), (1, -1), (1, 1), (-1, 1)).map{point => cellAt(point._1, point._2)}
		}

		override def toString = data.mkString

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data

		override def hashCode() = data.hashCode()
	}
}