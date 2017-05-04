package katas.scala.game_of_life

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

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
			""").next() should equalTo(new Field("""
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
			""").next() should equalTo(new Field("""
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
			""").next() should equalTo(new Field("""
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
			""").next().cellAt(1, 1).toString should equalTo("-")
	}

	@Test def gettingCellShouldWrapAroundFieldBorder() {
		val field = new Field(
			"""
			  |--0
			  |-0-
			  |0--
			""")
		field.cellAt(0, 0) should equalTo(DeadCell)
		field.cellAt(0, 1) should equalTo(DeadCell)
		field.cellAt(0, 2) should equalTo(LivingCell)
		field.cellAt(-1, 0) should equalTo(LivingCell)
		field.cellAt(-2, 0) should equalTo(DeadCell)
		field.cellAt(-3, 0) should equalTo(DeadCell)

		field.cellAt(-1, -1) should equalTo(DeadCell)
		field.cellAt(-2, -2) should equalTo(LivingCell)
		field.cellAt(0, 5) should equalTo(LivingCell)
	}

	case class Cell(c: Char) {
		override def toString = c.toString
	}
	object LivingCell extends Cell('0')
	object DeadCell extends Cell('-')
	object UndefinedCell extends Cell(' ')

	class Field(private val data: List[List[Cell]]) {

		def this(s: String) {
			this(s.stripMargin.trim.split("\n").map{_.toList.map{ Cell(_) }}.toList)
		}

		def next(): Field = {
			var newData: List[List[Cell]] = List.fill(data.size){ List.fill(data.size){ UndefinedCell } }
			for (row <- 0 until data.size; col <- 0 until data.size) {
				val liveCellsAround = cellsAround(row, col).count{_ == LivingCell}
				val newCellState =
					if (liveCellsAround < 2 || liveCellsAround > 3) DeadCell
					else LivingCell
				newData = newData.updated(row, newData(row).updated(col, newCellState))
			}
			new Field(newData)
		}

		def cellAt(row: Int, col: Int): Cell = {
			def wrap(n: Int) = (n + data.size) % data.size
			data(wrap(row))(wrap(col))
		}

		private def cellsAround(row: Int, col: Int): Seq[Cell] = {
			List((-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1)).map{ point =>
				cellAt(row + point._1, col + point._2)
			}
		}

		override def toString = "\n" + data.map{_.mkString}.mkString("\n") + "\n"

		override def equals(that: Any) = that.isInstanceOf[Field] && that.asInstanceOf[Field].data == data
	}
}