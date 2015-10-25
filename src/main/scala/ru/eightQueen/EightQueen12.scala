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

		var position = Position.none(boardSize)
		while (!position.isLast(boardSize)) {
			position = position.next(boardSize)
			allPositions += position
		}

		val expectedPositions: Seq[Position] = Seq(
			(0, 0), (0, 1), (0, 2),
			(1, 0), (1, 1), (1, 2),
			(2, 0), (2, 1), (2, 2)
		)
		allPositions should equal(expectedPositions)
	}

	private def findPositions(boardSize: BoardSize): PositionsSet = {
		findPositions(boardSize, Position.none(boardSize), Positions.none())
	}

	private def findPositions(boardSize: BoardSize, position: Position, result: Positions): PositionsSet = {
		if (position.isLast(boardSize)) {
			return if (result.isComplete(boardSize)) PositionsSet(result) else PositionsSet.empty()
		}
		val nextPosition = position.next(boardSize)
		val newLinePosition = nextPosition.newLine()
		if (result.add(nextPosition).isValid) {
			findPositions(boardSize, newLinePosition, result.add(nextPosition)) ++
			findPositions(boardSize, nextPosition, result)
		} else {
			findPositions(boardSize, nextPosition, result)
		}
	}

	private case class BoardSize(value: Int) {
		def maxRow = Row(value - 1)
		def maxColumn = Column(value - 1)
	}

	private case class Row(value: Int) extends Ordered[Row] {
		override def compare(that: Row): Int = this.value.compare(that.value)
		def next = Row(value + 1)
		def distanceTo(that: Row): Int = (this.value - that.value).abs
	}
	private object Row {
		val none = Row(-1)
	}
	private case class Column(value: Int) extends Ordered[Column] {
		override def compare(that: Column): Int = this.value.compare(that.value)
		def next = Column(value + 1)
		def distanceTo(that: Column): Int = (this.value - that.value).abs
	}
	private object Column {
		val none = Column(-1)
		val zero = Column(0)
	}


	private case class Position(row: Row, column: Column) {
		def next(boardSize: BoardSize): Position = {
			if (column == boardSize.maxColumn) Position(row.next, Column.zero) else Position(row, column.next)
		}
		def isLast(boardSize: BoardSize): Boolean = {
			(row == boardSize.maxRow && column == boardSize.maxColumn) || row > boardSize.maxRow
		}
		def newLine(): Position = {
			Position(row.next, Column.none)
		}
		def onSameRowOrDiagonal(that: Position): Boolean = {
			(this.row == that.row || this.column == that.column) ||
			(this.row.distanceTo(that.row) == this.column.distanceTo(that.column))
		}
	}
	private object Position {
		def none(boardSize: BoardSize): Position = Position(Row.none, boardSize.maxColumn)
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
		def none(): Positions = {
			Positions(Seq())
		}
		implicit def toSeq(positions: Positions): Seq[Position] = {
			positions.value
		}
	}
}