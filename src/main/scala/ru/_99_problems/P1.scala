package ru._99_problems

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class P1 extends ShouldMatchers {
	def encode[T](seq: Seq[T]): Seq[(Int, T)] = {
		new P0().pack(seq).map{ it => (it.size, it.head) }
	}

	@Test def `P10 (*) Run-length encoding of a list.`() {
		encode(Seq()) should equal(Seq())
		encode(Seq('a)) should equal(Seq((1, 'a)))
		encode(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), (1, 'b)))
	}

	@Test def `P11 (*) Modified run-length encoding.`() {
		def encodeModified[T](seq: Seq[T]): Seq[Any] = {
			new P1().encode(seq).map{
			  case (count, item) if count == 1 => item
			  case (count, item) if count > 1 => (count, item)
			}
		}

		encodeModified(Seq()) should equal(Seq())
		encodeModified(Seq('a)) should equal(Seq('a))
		encodeModified(Seq('a, 'b)) should equal(Seq('a, 'b))
		encodeModified(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), 'b))
	}

	@Test def `P12 (**) Decode a run-length encoded list.`() {
		def decode[T](seq: Seq[(Int, T)]): Seq[T] = {
			seq.flatMap{ entry => Seq.fill(entry._1)(entry._2) }
		}

		decode(Seq()) should equal(Seq())
		decode(Seq((1, 'a))) should equal(Seq('a))
		decode(Seq((2, 'a), (1, 'b))) should equal(Seq('a, 'a, 'b))
	}

	@Test def `P13 (**) Run-length encoding of a list (direct solution).`() {
		def encodeDirect[T](seq: Seq[T], lastItem: T = Nil, count: Int = 0): Seq[(Int, T)] = {
			def doEncode(seq: Seq[T], lastItem: T, count: Int): Seq[(Int, T)] = {
				if (seq.isEmpty) Seq((count, lastItem))
				else if (seq.head == lastItem) doEncode(seq.tail, lastItem, count + 1)
				else (count, lastItem) +: doEncode(seq.tail, seq.head, 1)
			}
			seq match {
				case Seq() => Seq()
				case x :: xs => doEncode(xs, x, 1)
			}
		}

		encodeDirect(Seq()) should equal(Seq())
		encodeDirect(Seq('a)) should equal(Seq((1, 'a)))
		encodeDirect(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), (1, 'b)))
	}

	@Test def `P14 (*) Duplicate the elements of a list.`() {
		def duplicate[T](seq: Seq[T]): Seq[T] = seq match {
			case Seq() => Seq()
			case x :: xs => Seq(x, x) ++ duplicate(xs)
		}
		duplicate(Seq()) should equal(Seq())
		duplicate(Seq('a)) should equal(Seq('a, 'a))
		duplicate(Seq('a, 'b)) should equal(Seq('a, 'a, 'b, 'b))
	}

	@Test def `P15 (**) Duplicate the elements of a list a given number of times.`() {
		def nTimes[T](value: T, n: Int): Seq[T] = n match {
			case 0 => Seq()
			case _ => value +: nTimes(value, n -1)
		}
		def duplicateN[T](times: Int, seq: Seq[T]): Seq[T] = seq match {
			case Seq() => Seq()
			case x :: xs => nTimes(x, times) ++ duplicateN(times, xs)
		}
		duplicateN(1, Seq()) should equal(Seq())
		duplicateN(1, Seq('a)) should equal(Seq('a))
		duplicateN(2, Seq('a)) should equal(Seq('a, 'a))
		duplicateN(2, Seq('a, 'b)) should equal(Seq('a, 'a, 'b, 'b))
	}

	@Test def `P16 (**) Drop every Nth element from a list.`() {
		def dropEvery[T](step: Int, seq: Seq[T], counter: Int = 1): Seq[T] = seq match {
			case Seq() => Seq()
			case x :: xs =>
				if (counter < step) x +: dropEvery(step, xs, counter + 1)
				else dropEvery(step, xs)
		}
		dropEvery(2, Seq()) should equal(Seq())
		dropEvery(1, Seq('a)) should equal(Seq())
		dropEvery(2, Seq('a)) should equal(Seq('a))
		dropEvery(1, Seq('a, 'b)) should equal(Seq())
		dropEvery(2, Seq('a, 'b)) should equal(Seq('a))
		dropEvery(2, Seq('a, 'b, 'c)) should equal(Seq('a, 'c))
	}

	@Test def `P17 (*) Split a list into two parts.`() {
		def split[T](size: Int, seq: Seq[T], firstPart: Seq[T] = Seq()): (Seq[T], Seq[T]) = {
			if (size == 0) (firstPart, seq)
			else split(size - 1, seq.tail, firstPart :+ seq.head)
		}
		split(0, Seq()) should equal(Seq(), Seq())
		split(0, Seq('a)) should equal(Seq(), Seq('a))
		split(1, Seq('a)) should equal(Seq('a), Seq())
		split(1, Seq('a, 'b)) should equal((Seq('a), Seq('b)))
		split(2, Seq('a, 'b)) should equal(Seq('a, 'b), Seq())
		split(2, Seq('a, 'b, 'c)) should equal((Seq('a, 'b), Seq('c)))
	}
}