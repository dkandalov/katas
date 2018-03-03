package katas.scala.game_of_life

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable

/**
 * User: dima
 * Date: 28/10/2012
 */

class Conway5 extends Matchers {
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

	class Field(val data: mutable.Buffer[mutable.Buffer[Char]]) { // needed data to be "val"
		def this(s: String) {
			this(s.trim.stripMargin.split("\n").toBuffer[String].map{_.toBuffer[Char]})
		}

		def cellAt(row: Int, col: Int): Char = {
			def wrap = { n: Int => (n + data.size) % data.size }
			data(wrap(row))(wrap(col)) // needed access with two "apply()"
		}

		def next(): Field = {
			val newData = mutable.Buffer.fill(data.size, data.size){' '} // should've been mutable.Buffer
			for (row <- data.indices; col <- data.indices) {
				def liveCellsAround = cellsAround(row, col).count{_ == '0'}
				val cellState =
					if (liveCellsAround < 2 || liveCellsAround > 3) '-'
					else '0'
				newData(row)(col) = cellState
			}
			new Field(newData)
		}

		private def cellsAround(row: Int, col: Int) = {
			Seq((-1, 0), (1, 0), (0, 1), (0, -1), (-1, -1), (1, -1), (1, 1), (-1, 1)).map{ point =>
				cellAt(row + point._1, col + point._2)
			}
		}

		override def toString: String = "\n" + data.map{_.mkString}.mkString("\n")

		override def equals(that: Any): Boolean = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data

		override def hashCode(): Int = data.hashCode()
	}
}