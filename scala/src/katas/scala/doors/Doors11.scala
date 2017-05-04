package katas.scala.doors

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class Doors11 extends ShouldMatchers {

	@Test def `walking doors`() {
		walkDoors(amount = 1) should equalTo(Seq(true))
		walkDoors(amount = 2) should equalTo(Seq(true, false))
		walkDoors(amount = 3) should equalTo(Seq(true, false, false))
		walkDoors(amount = 4) should equalTo(Seq(true, false, false, true))
		walkDoors(amount = 5) should equalTo(Seq(true, false, false, true, false))
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