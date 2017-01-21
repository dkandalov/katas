package ru.doors

import org.scalatest.Matchers
import org.junit.Test


class Doors11 extends Matchers {

	@Test def `walking doors`() {
		walkDoors(amount = 1) should equal(Seq(true))
		walkDoors(amount = 2) should equal(Seq(true, false))
		walkDoors(amount = 3) should equal(Seq(true, false, false))
		walkDoors(amount = 4) should equal(Seq(true, false, false, true))
		walkDoors(amount = 5) should equal(Seq(true, false, false, true, false))
	}

	private def walkDoors(amount: Int): Seq[Boolean] = {
		val doors = Array.fill(amount){ false }
		Range.inclusive(1, amount).foreach{ stepSize =>
			Range(stepSize - 1, amount, stepSize).foreach { i =>
				doors(i) = !doors(i)
			}
		}
		doors.toSeq
	}
}