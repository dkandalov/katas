package katas.scala.eightQueen

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
		while (boardSize.includes(position)) {
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

	private def findPositions(boardSize: BoardSize): PositionSets = {
		findPositions(boardSize, Position.zero, PositionSet.empty())
	}

	private def findPositions(boardSize: BoardSize, position: Position, positions: PositionSet): PositionSets = {
		if (!boardSize.includes(position)) {
			return if (boardSize.isCompletedWith(positions)) PositionSets(positions) else PositionSets.empty()
		}
		if (positions.add(position).isValid) {
			findPositions(boardSize, position.newLine(), positions.add(position)) ++
			findPositions(boardSize, boardSize.next(position), positions)
		} else {
			findPositions(boardSize, boardSize.next(position), positions)
		}
	}


	private case class BoardSize(private val size: Int) {
		def includes(position: Position): Boolean = {
			position.row < Row(size)
		}
		def isCompletedWith(positions: PositionSet): Boolean = {
			positions.size == size
		}
		def next(position: Position): Position = {
			if (isLast(position.column)) Position(position.row.next, Column.zero)
			else Position(position.row, position.column.next)
		}
		private def isLast(column: Column) = column == Column(size - 1)
	}


	private case class Row(private val value: Int) extends Ordered[Row] {
		override def compare(that: Row): Int = this.value.compare(that.value)
		def next = Row(value + 1)
		def distanceTo(that: Row): Int = (this.value - that.value).abs
	}
	private object Row {
		val zero = Row(0)
	}


	private case class Column(private val value: Int) extends Ordered[Column] {
		override def compare(that: Column): Int = this.value.compare(that.value)
		def next = Column(value + 1)
		def distanceTo(that: Column): Int = (this.value - that.value).abs
	}
	private object Column {
		val zero = Column(0)
	}


	private case class Position(row: Row, column: Column) {
		def newLine(): Position = {
			Position(row.next, Column.zero)
		}
		def onTheSameRowOrDiagonal(that: Position): Boolean = {
			(this.row == that.row || this.column == that.column) ||
			(this.row.distanceTo(that.row) == this.column.distanceTo(that.column))
		}
	}
	private object Position {
		val zero = Position(Row.zero, Column.zero)
		implicit def tupleToBoardPosition(tuple: (Int, Int)): Position = Position(Row(tuple._1), Column(tuple._2))
	}


	private case class PositionSet(private val value: Seq[Position]) {
		def add(position: Position): PositionSet = {
			PositionSet(position +: value)
		}
		def isValid: Boolean = {
			value.forall{ thisPosition =>
				value.filter(_ != thisPosition).forall {
					!thisPosition.onTheSameRowOrDiagonal(_)
				}
			}
		}
		def size: Int = value.size
	}
	private object PositionSet {
		def empty(): PositionSet = {
			PositionSet(Seq())
		}
		implicit def toSeq(positions: PositionSet): Seq[Position] = {
			positions.value
		}
	}


	private case class PositionSets(private val value: Seq[PositionSet]) {
		def ++(that: PositionSets): PositionSets = {
			PositionSets(this.value ++ that.value)
		}
	}
	private object PositionSets {
		def apply(positions: PositionSet): PositionSets = PositionSets(Seq(positions))
		def empty(): PositionSets = PositionSets(Seq())
		implicit def toSequence(positionsSet: PositionSets): Seq[PositionSet] = positionsSet.value
	}
}