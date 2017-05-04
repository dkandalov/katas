package katas.scala.newton

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class Newton8 extends ShouldMatchers {
	@Test def `find square root`() {
		sqrt(1) should beTolerantEqualTo(1)
		sqrt(2) should beTolerantEqualTo(1.414)
		sqrt(3) should beTolerantEqualTo(1.732)
		sqrt(4) should beTolerantEqualTo(2)
	}

	private def sqrt(n: Double, r: Double = 1, tolerance: Double = 0.001): Double = {
		if ((n - (r * r)).abs < tolerance) r
		else {
			val improvedResult = r - ((r * r - n) / (2 * r))
			sqrt(n, improvedResult, tolerance)
		}
	}
}