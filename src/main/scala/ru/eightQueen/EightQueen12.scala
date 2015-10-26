package ru.eightQueen

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer


class EightQueen12 extends Matchers {

	@Test def `find queen positions for different board sizes`() {
		findPositions(BoardSize(1)).size should equal(1)
		findPositions(BoardSize(2)).size should equal(0)
		findPositions(BoardSize(3)).size should equal(0)
		findPositions(BoardSize(4)).size should equal(2)
		findPositions(BoardSize(5)).size should equal(10)
		findPositions(BoardSize(8)).size should equal(92)
	}

	@Test def `generate all positions on the board`() {
		val allPositions = ListBuffer[Position]()
		val boardSize = BoardSize(3)

		var position = Position.zero
		while (!position.isAfterLast(boardSize)) {
			allPositions += position
			position = boardSize.next(position)
		}

		val expectedPositions: Seq[Position] = Seq(
			(0, 0), (0, 1), (0, 2),
			(1, 0), (1, 1), (1, 2),
			(2, 0), (2, 1), (2, 2)
		)
		allPositions should equal(expectedPositions)
	}

	private def findPositions(boardSize: BoardSize): PositionsSet = {
		findPositions(boardSize, Position.zero, Positions.empty())
	}

	private def findPositions(boardSize: BoardSize, position: Position, result: Positions): PositionsSet = {
		if (position.isAfterLast(boardSize)) {
			return if (result.isComplete(boardSize)) PositionsSet(result) else PositionsSet.empty()
		}
		if (result.add(position).isValid) {
			findPositions(boardSize, position.newLine(), result.add(position)) ++
			findPositions(boardSize, boardSize.next(position), result)
		} else {
			findPositions(boardSize, boardSize.next(position), result)
		}
	}

	private case class BoardSize(value: Int) {
		def isLast(column: Column) = column.value == value - 1
		def isAfterLast(row: Row) = row.value > value - 1
		def next(position: Position) = {
			if (isLast(position.column)) Position(position.row.next, Column.zero)
			else Position(position.row, position.column.next)
		}
	}

	private case class Row(value: Int) extends Ordered[Row] {
		override def compare(that: Row): Int = this.value.compare(that.value)
		def next = Row(value + 1)
		def distanceTo(that: Row): Int = (this.value - that.value).abs
	}
	private object Row {
		val zero = Row(0)
	}
	private case class Column(value: Int) extends Ordered[Column] {
		override def compare(that: Column): Int = this.value.compare(that.value)
		def next = Column(value + 1)
		def distanceTo(that: Column): Int = (this.value - that.value).abs
	}
	private object Column {
		val zero = Column(0)
	}


	private case class Position(row: Row, column: Column) {
		def isAfterLast(boardSize: BoardSize): Boolean = {
			boardSize.isAfterLast(row)
		}
		def newLine(): Position = {
			Position(row.next, Column.zero)
		}
		def onSameRowOrDiagonal(that: Position): Boolean = {
			(this.row == that.row || this.column == that.column) ||
			(this.row.distanceTo(that.row) == this.column.distanceTo(that.column))
		}
	}
	private object Position {
		val zero = Position(Row.zero, Column.zero)
		implicit def tupleToBoardPosition(tuple: (Int, Int)): Position = Position(Row(tuple._1), Column(tuple._2))
	}

	private case class PositionsSet(value: Seq[Positions]) {
		def ++(that: PositionsSet): PositionsSet = {
			PositionsSet(this.value ++ that.value)
		}
	}
	private object PositionsSet {
		def apply(positions: Positions): PositionsSet = PositionsSet(Seq(positions))
		def empty(): PositionsSet = PositionsSet(Seq())
		implicit def toSequence(positionsSet: PositionsSet): Seq[Positions] = positionsSet.value
	}

	private case class Positions(value: Seq[Position]) {
		def add(position: Position): Positions = {
			Positions(position +: value)
		}
		def isValid: Boolean = {
			value.forall{ thisPosition =>
				value.filter(_ != thisPosition).forall {
					!thisPosition.onSameRowOrDiagonal(_)
				}
			}
		}
		def isComplete(boardSize: BoardSize): Boolean = {
			value.size == boardSize.value
		}
	}

	private object Positions {
		def empty(): Positions = {
			Positions(Seq())
		}
		implicit def toSeq(positions: Positions): Seq[Position] = {
			positions.value
		}
	}
}