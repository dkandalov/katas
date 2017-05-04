package katas.scala.sieve

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class SOE3 extends ShouldMatchers {
	@Test def `finding prime numbers`() {
		findPrimes(10) should equalTo(Seq(1, 2, 3, 5, 7))
		findPrimes(20) should equalTo(Seq(1, 2, 3, 5, 7, 11, 13, 17, 19))
		findPrimes(50) should equalTo(Seq(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47))
	}

	private def findPrimes(n: Int): Seq[Int] = {
		var primes = Seq[Int]()
		def isPrime(i: Int) = primes.forall{i % _ != 0}
		Range(2, n + 1).foreach { i =>
			if (isPrime(i)) primes = primes :+ i
		}
		1 +: primes
	}
}