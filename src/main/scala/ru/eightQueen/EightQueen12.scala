package ru.eightQueen

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer


class EightQueen12 extends Matchers {

	@Test def `find queen positions for different board sizes`() {
		findPositions(boardSize = 1).size should equal(1)
		findPositions(boardSize = 2).size should equal(0)
		findPositions(boardSize = 3).size should equal(0)
		findPositions(boardSize = 4).size should equal(2)
		findPositions(boardSize = 5).size should equal(10)
		findPositions(boardSize = 8).size should equal(92)
	}

	@Test def `generate all positions on the board`() {
		val allPositions = ListBuffer[Position]()
		val boardSize = 3

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

	private def findPositions(boardSize: Int): Seq[Seq[Position]] = {
		findPositions(boardSize, Position.none(boardSize), Positions.none())
				.filter(it => it.isComplete(boardSize))
				.map(_.toSeq)
	}

	private def findPositions(boardSize: Int, position: Position, result: Positions): Seq[Positions] = {
		if (position.isLast(boardSize)) return Seq(result)
		val nextPosition = position.next(boardSize)
		val newLinePosition = nextPosition.newLine()
		if (result.add(nextPosition).isValid) {
			findPositions(boardSize, newLinePosition, result.add(nextPosition)) ++
			findPositions(boardSize, nextPosition, result)
		} else {
			findPositions(boardSize, nextPosition, result)
		}
	}

	private case class Position(row: Int, column: Int) {
		def next(boardSize: Int): Position = {
			if (column == boardSize - 1) (row + 1, 0) else (row, column + 1)
		}

		def isLast(boardSize: Int): Boolean = {
			(row == boardSize - 1 && column == boardSize - 1) || row > boardSize - 1
		}

		def newLine(): Position = {
			(row + 1, -1)
		}

		def onSameRowOrDiagonal(that: Position): Boolean = {
			(this.row == that.row || this.column == that.column) ||
			((this.row - that.row).abs == (this.column - that.column).abs)
		}
	}

	private object Position {
		def none(boardSize: Int): Position = {
			Position(-1, boardSize - 1)
		}

		implicit def tupleToBoardPosition(tuple: (Int, Int)): Position = {
			Position(tuple._1, tuple._2)
		}
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

		def isComplete(boardSize: Int): Boolean = {
			value.size == boardSize
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