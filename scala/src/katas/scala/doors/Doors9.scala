package katas.scala.doors

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class Doors9 extends ShouldMatchers {
	@Test def `state of doors after walking`() {
		walkDoors(amount = 1) should equalTo(Seq(true))
		walkDoors(amount = 2) should equalTo(Seq(true, false))
		walkDoors(amount = 3) should equalTo(Seq(true, false, false))
		walkDoors(amount = 4) should equalTo(Seq(true, false, false, true))
		walkDoors(amount = 5) should equalTo(Seq(true, false, false, true, false))
		walkDoors(amount = 6) should equalTo(Seq(true, false, false, true, false, false))
		walkDoors(amount = 100).zipWithIndex should equalTo(walkDoors2(100).zipWithIndex)
	}

	private def walkDoors(amount: Int): Seq[Boolean] = {
		val doors = Array.fill(amount){ false }
		val doorsToWalk = 1.to(amount).flatMap{ stepSize =>
			(stepSize - 1).to(amount - 1, stepSize)
		}
		doorsToWalk.foreach{ i => doors(i) = !doors(i) }
		doors.toSeq
	}

	private def walkDoors2(amount: Int): Seq[Boolean] = {
		val doors = Array.fill(amount){ false }
		var i = 0
		var stepsToSkip = 0
		while (i < amount) {
			doors(i) = true
			stepsToSkip = stepsToSkip + 2
			i = i + stepsToSkip + 1
		}
		doors.toSeq
	}
}