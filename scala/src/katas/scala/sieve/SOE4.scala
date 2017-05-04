package katas.scala.sieve

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class SOE4 extends ShouldMatchers {
	@Test def `find prime numbers`() {
		primesUpTo(1) should equalTo(Seq())
		primesUpTo(2) should equalTo(Seq())
		primesUpTo(3) should equalTo(Seq(2))
		primesUpTo(10) should equalTo(Seq(2, 3, 5, 7))
		primesUpTo(10) should equalTo(Seq(2, 3, 5, 7))
		primesUpTo(100) should equalTo(Seq(
			2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
		)
	}

	private def primesUpTo(n: Int): Seq[Int] = {
		if (n < 2) return Seq()

		val data = Array.fill(n){true}
		var i = 2
		var result = Seq[Int]()
		while (i < n) {
			if (data(i)) {
				result = result :+ i
				i.to(n-1, i).foreach{data(_) = false}
			}
			i = i + 1
		}
		result
	}
}