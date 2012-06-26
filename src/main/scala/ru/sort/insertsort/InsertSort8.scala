package ru.sort.insertsort

import ru.util.Arcade
import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 26/06/2012
 */

@Arcade
class InsertSort8 extends ShouldMatchers {
	@Test def sortIntegerSequencesOfDifferentSize() {
		sort(Seq()) should equal(Seq())
		sort(Seq(1)) should equal(Seq(1))
		sort(Seq(1, 2)) should equal(Seq(1, 2))
		sort(Seq(2, 1)) should equal(Seq(1, 2))
		sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(2, 1, 3)) should equal(Seq(1, 2, 3))
		sort(Seq(3, 1, 2)) should equal(Seq(1, 2, 3))
		
		1.to(8).map(Range(1, _).toSeq).foreach { seq =>
			seq.permutations.foreach{ perm =>
				sort(perm) should equal(seq)
			}
		}
	}

	def sort(seq: Seq[Int]): Seq[Int] = {
		Seq()
	}
}