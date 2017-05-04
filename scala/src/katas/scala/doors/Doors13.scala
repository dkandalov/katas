package katas.scala.doors

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class Doors13 extends ShouldMatchers {
	@Test def `walking doors`() {
		val amount = 10
		val steps = Range.inclusive(1, amount).flatMap { stepSize =>
			Range(stepSize - 1, amount, stepSize).toSeq
		}
		val doors = steps.groupBy{ it => it }.toList.sortBy(_._1).map{ case (_, seq) => seq.size % 2 == 1}

		doors should equalTo(Seq(true, false, false, true, false, false, false, false, true, false))
	}
}