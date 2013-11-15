package org.techstock

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import scala.annotation.tailrec


class Primes extends ShouldMatchers {
	@Test def `find sum of primes below 2 million`() {
		// too slow for 2M
		def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
		def sieve(s: Stream[Int]): Stream[Int] =
			Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0}))
		def primes = sieve(from(2))
		primes.take(100).sum should equal(24133)

		// too slow for 2M
		@tailrec def primes2(amount: Int = 100, n: Int = 2, result: Seq[Int] = Seq()): Seq[Int] = {
			if (n >= amount) result
			else if (result.isEmpty || result.forall{ n % _ != 0 }) primes2(amount, n + 1, n +: result)
			else primes2(amount, n + 1, result)
		}
		primes2(100).sum should equal(1060)
	}
}