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
		while (!isLast(boardSize, position)) {
			position = nextPosition(boardSize, position)
			allPositions += position
		}

		allPositions should equal(Seq(
			(0,0), (0,1), (0,2),
			(1,0), (1,1), (1,2),
			(2,0), (2,1), (2,2)
		))
	}

	private def findPositions(boardSize: Int): Seq[Seq[Position]] = {
		findPositions(boardSize, (-1, boardSize - 1), Seq()).filter(it => isComplete(boardSize, it))
	}

	private def findPositions(boardSize: Int, position: Position, result: Seq[Position]): Seq[Seq[Position]] = {
		if (isLast(boardSize, position)) return Seq(result)
		val newPosition = nextPosition(boardSize, position)
		val newLinePosition = newLine(newPosition)
		if (isValid(newPosition +: result)) {
			findPositions(boardSize, newLinePosition, newPosition +: result) ++
			findPositions(boardSize, newPosition, result)
		} else {
			findPositions(boardSize, newPosition, result)
		}
	}

	private case class Position(row: Int, column: Int)

	private implicit def tupleToBoardPosition(tuple: (Int, Int)): Position = {
		Position(tuple._1, tuple._2)
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

	private def isLast(boardSize: Int, position: Position): Boolean = {
		(position.row == boardSize - 1 && position.column == boardSize - 1) ||
		 position.row > boardSize - 1
	}

	private def nextPosition(boardSize: Int, position: Position): Position = {
		if (position.column == boardSize - 1) (position.row + 1, 0) else (position.row, position.column + 1)
	}

	private def newLine(position: Position): Position = {
		(position.row + 1, -1)
	}
}