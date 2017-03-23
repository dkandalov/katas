package katas.scala.sieve

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer


class SOE5 extends Matchers {
	@Test def `find prime numbers`() {
		primes().take(10) should equal(Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
		iteratorOfPrimes().take(10).toList should equal(Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
		iteratorOfPrimes().toStream.take(10) should equal(Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
	}

	private def primes(): Stream[Int] = {
		def streamOfPrimes(n: Int): Stream[Int] = {
			Stream(n) append streamOfPrimes(n + 1).filter{ _ % n != 0 }
		}
		streamOfPrimes(2)
	}

	private def iteratorOfPrimes(): Iterator[Int] = {
		new Iterator[Int]() {
			private var n = 2
			private val primes = ListBuffer[Int]()

			override def next() = {
				while (primes.exists{n % _ == 0}) n = n + 1
				primes.append(n)
				n
			}
			override def hasNext = true
		}
	}
}