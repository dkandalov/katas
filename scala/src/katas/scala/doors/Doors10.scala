package katas.scala.doors

import org.junit.Test
import org.scalatest.Matchers


class Doors10 extends Matchers {
	@Test def `walking doors`() {
		walkDoors(amount = 1) should equal(Seq(open))
		walkDoors(amount = 2) should equal(Seq(open, closed))
		walkDoors(amount = 10) should equal(Seq(open, closed, closed, open, closed, closed, closed, closed, open, closed))
	}

	private class DoorState
	private val open = new DoorState() 
	private val closed = new DoorState()

	private def walkDoors(amount: Int): Seq[DoorState] = {
		val doorsToWalk = Range.inclusive(1, amount).flatMap{ stepSize =>
			Range(stepSize - 1, amount, stepSize)
		}
		val doors = Array.fill[DoorState](amount){ closed }
		doorsToWalk.foreach{ i =>
			doors(i) = if (doors(i) == open) closed else open
		}
		doors.toSeq
	}
}