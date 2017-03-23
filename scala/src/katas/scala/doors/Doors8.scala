package katas.scala.doors

import org.junit.Test
import org.scalatest.Matchers


class Doors8 extends Matchers {
	@Test def `end state of doors`() {
		walkDoors(amount = 1) should equal(Seq(true))
		walkDoors(amount = 2) should equal(Seq(true, false))
		walkDoors(amount = 3) should equal(Seq(true, false, false))
		walkDoors(amount = 4) should equal(Seq(true, false, false, true))
		walkDoors(amount = 5) should equal(Seq(true, false, false, true, false))
		walkDoors(amount = 6) should equal(Seq(true, false, false, true, false, false))
		walkDoors(amount = 7) should equal(Seq(true, false, false, true, false, false, false))
		walkDoors(amount = 8) should equal(Seq(true, false, false, true, false, false, false, false))
	}

	private def walkDoors(amount: Int): Seq[Boolean] = {
		val doors = Array.fill(amount){ false }
		1.to(amount).foreach{ stepSize =>
			Range(stepSize - 1, amount, stepSize).foreach { i =>
				doors(i) = !doors(i)
			}
		}
		doors.toSeq
	}
}