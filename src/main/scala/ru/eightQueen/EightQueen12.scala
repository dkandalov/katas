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

		var position = Position(-1, boardSize - 1)
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
		findPositions(boardSize, Position.none(boardSize), Seq()).filter(it => isComplete(boardSize, it))
	}

	private def findPositions(boardSize: Int, position: Position, result: Seq[Position]): Seq[Seq[Position]] = {
		if (position.isLast(boardSize)) return Seq(result)
		val nextPosition = position.next(boardSize)
		val newLinePosition = nextPosition.newLine()
		if (isValid(nextPosition +: result)) {
			findPositions(boardSize, newLinePosition, nextPosition +: result) ++
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
	}

	private object Position {
		def none(boardSize: Int): Position = {
			Position(-1, boardSize - 1)
		}

		implicit def tupleToBoardPosition(tuple: (Int, Int)): Position = {
			Position(tuple._1, tuple._2)
		}
	}


	private def isComplete(boardSize: Int, positions: Seq[Position]): Boolean = {
		positions.size == boardSize
	}

	private def isValid(positions: Seq[Position]): Boolean = {
		positions.forall{ thisPosition =>
			positions.filter(_ != thisPosition).forall { thatPosition =>
				(thisPosition.row != thatPosition.row && thisPosition.column != thatPosition.column) &&
				((thisPosition.row - thatPosition.row).abs != (thisPosition.column - thatPosition.column).abs)
			}
		}
	}
}