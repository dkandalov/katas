package katas.scala.hanoi

import org.junit.Test
import org.scalatest.Matchers


class Hanoi7 extends Matchers {
	@Test def `find moves to solve Towers of Hanoi problem`() {
		findMoves(towerSize = 1) should equal(Seq(Move(0, -1)))
		findMoves(towerSize = 2) should equal(Seq(Move(0, 1), Move(1, -1), Move(0, 1)))
		findMoves(towerSize = 3) should equal(Seq(
			Move(0, -1), Move(1, 1), Move(0, -1),
			Move(2, -1),
			Move(0, -1), Move(1, 1), Move(0, -1)
		))
	}

	private case class Move(towerIndex: Int, shift: Int)

	private def findMoves(towerSize: Int): Seq[Move] = {
		def shiftStream: Stream[Int] = Seq(-1, 1).toStream #::: shiftStream
		val shifts = shiftStream.take(towerSize).toList
		val moves = Range(towerSize-1, -1, -1).zip(shifts).map{ it => Move(it._1, it._2) }
		moves.foldRight(Seq[Move]()) { (move, result) =>
			result ++ Seq(move) ++ result
		}
	}
}