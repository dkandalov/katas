package katas.scala.doors

import org.junit.Test
import org.scalatest.Matchers


class Doors13 extends Matchers {
	@Test def `walking doors`() {
		val amount = 10
		val steps = Range.inclusive(1, amount).flatMap { stepSize =>
			Range(stepSize - 1, amount, stepSize).toSeq
		}
		val doors = steps.groupBy{ it => it }.toList.sortBy(_._1).map{ case (_, seq) => seq.size % 2 == 1}

		doors should equal(Seq(true, false, false, true, false, false, false, false, true, false))
	}
}