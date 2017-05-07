package katas.scala.game_of_life

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable

/**
 * User: dima
 * Date: 27/10/2012
 */

class Conway4 extends Matchers {

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

	@Test def overpopulatedCellDies() {
		new Field(
			"""
			  |0-0
			  |-0-
			  |0-0
			""").next().cellAt(1, 1) should equal('-')
	}

	class Field(val data: mutable.Buffer[mutable.Buffer[Char]]) {

		def this(s : String) {
			this(s.trim.stripMargin.split("\n").toBuffer[String].map{ _.toBuffer[Char] })
		}

		def cellAt(row: Int, col: Int): Char = {
			def wrap = { n: Int => (n + data.size) % data.size}
			data(wrap(row))(wrap(col))
		}

		def next(): Field = {
			val newData = mutable.Buffer.fill(data.size, data.size){ ' ' }

			for (row <- 0 until data.size; col <- 0 until data.size) {
				val liveCellsAround = cellsAround(row, col).count{_ == '0'}
				val cellState =
					if (liveCellsAround < 2 || liveCellsAround > 3) '-'
					else '0'
				newData(row)(col) = cellState
			}
			new Field(newData)
		}

		private def cellsAround(row: Int, col: Int): Seq[Char] = {
			Seq((-1, 0), (0, 1), (0, 1), (0, -1), (-1, -1), (1, -1), (1, 1), (-1, 1)).map{point => cellAt(row + point._1, col + point._2)}
		}

		override def toString = data.mkString

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data

		override def hashCode() = data.hashCode()
	}
}