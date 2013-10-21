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
}