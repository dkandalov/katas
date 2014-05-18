package ru.sieve

import org.scalatest.Matchers
import org.junit.Test


class SOE5 extends Matchers {
	@Test def `find prime numbers`() {
		primes().take(10) should equal(Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
	}

	private def primes(): Stream[Int] = {
		def streamOfPrimes(n: Int): Stream[Int] = {
			Stream(n) append streamOfPrimes(n + 1).filter{ _ % n != 0 }
		}
		streamOfPrimes(2)
	}
}