package ru._99_problems

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class P1 extends ShouldMatchers {
	@Test def `P10 (*) Run-length encoding of a list.`() {
		def pack[T](seq: Seq[T], group: Seq[T] = Seq()): Seq[Seq[T]] = {
			if (seq.isEmpty) {
				if (group.isEmpty) Seq() else Seq(group)
			} else {
				if (group.isEmpty || group.head == seq.head) {
					pack(seq.tail, group :+ seq.head)
				} else {
					group +: pack(seq.tail, Seq(seq.head))
				}
			}
		}

		def encode[T](seq: Seq[T]): Seq[(Int, T)] = {
			pack(seq).map{ it => (it.size, it.head) }
		}

		encode(Seq()) should equal(Seq())
		encode(Seq('a)) should equal(Seq((1, 'a)))
		encode(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), (1, 'b)))
	}
}