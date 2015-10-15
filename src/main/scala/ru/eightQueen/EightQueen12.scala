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
		val allPositions = ListBuffer[(Int, Int)]()
		val boardSize = 3

		var position = (-1, boardSize - 1)
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

	private def findPositions(boardSize: Int): Seq[Seq[(Int, Int)]] = {
		findPositions(boardSize, (-1, boardSize - 1), Seq()).filter(it => isComplete(boardSize, it))
	}

	private def findPositions(boardSize: Int, position: (Int, Int), result: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] = {
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

	private def isComplete(boardSize: Int, positions: Seq[(Int, Int)]): Boolean = {
		positions.size == boardSize
	}

	private def isValid(positions: Seq[(Int, Int)]): Boolean = {
		positions.forall{ thisPosition =>
			positions.filter(_ != thisPosition).forall { thatPosition =>
				(thisPosition._1 != thatPosition._1 && thisPosition._2 != thatPosition._2) &&
				((thisPosition._1 - thatPosition._1).abs != (thisPosition._2 - thatPosition._2).abs)
			}
		}
	}

	private def isLast(boardSize: Int, position: (Int, Int)): Boolean = {
		(position._1 == boardSize - 1 && position._2 == boardSize - 1) ||
		 position._1 > boardSize - 1
	}

	private def nextPosition(boardSize: Int, position: (Int, Int)): (Int, Int) = {
		if (position._2 == boardSize - 1) (position._1 + 1, 0) else (position._1, position._2 + 1)
	}

	private def newLine(position: (Int, Int)): (Int, Int) = {
		(position._1 + 1, -1)
	}
}