package ru.streams

import org.scalatest.Matchers
import org.junit.Test

/**
 * User: dima
 * Date: 09/11/2012
 */

class Streams0 extends Matchers {
	@Test def test() {
		from(1).toString() should equal("Stream(1, ?)")
		from(1).tail.toString() should equal("Stream(3, ?)")
		from(1).take(10).toList should equal(List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19))

		val stream = new MyFromStream(1, {_ + 2})
		stream.take() should equal(1)
		stream.take() should equal(3)
		stream.take() should equal(5)
		stream.take() should equal(7)
		stream.take() should equal(9)
	}

	def from(n: Int): Stream[Int] = {
		Stream.cons(n, from(n + 2))
	}

	class MyFromStream(var n: Int, next: Int => Int) {
		def take(): Int = {
			val result = n
			n = next(n)
			result
		}
	}
}