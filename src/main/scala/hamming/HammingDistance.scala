package hamming

import org.junit.Test
import org.scalatest.Matchers


class HammingDistance extends Matchers {

	// https://en.wikipedia.org/wiki/Hadamard_code
	// https://en.wikipedia.org/wiki/Hamming_distance
	@Test def `find longest combination of 6-bit words with Hamming distance >= 3`(): Unit = {
		def distance(a: Int, b: Int): Int = {
			var result = 0
			var c = a ^ b
			while (c != 0) {
				if ((c & 1) == 1) result = result + 1
				c = c >> 1
			}
			result
		}
		distance(0, 0) should equal(0)
		distance(0, 1) should equal(1)
		distance(0, 2) should equal(1)
		distance(0, 3) should equal(2)
		distance(1, 3) should equal(1)


		def intToBinary(n: Int, width: Int): String = {
			n.toBinaryString.reverse.padTo(width, "0").reverse.mkString
		}
		intToBinary(0, 6) should equal("000000")
		intToBinary(1, 6) should equal("000001")
		intToBinary(11, 6) should equal("001011")
		intToBinary(33, 6) should equal("100001")


		def search(options: List[Int], solution: List[Int]): List[List[Int]] = {
			val newOptions = options.filter { option => solution.forall{ distance(_, option) >= 3 } }
			if (newOptions.isEmpty) return List(solution)

			newOptions.flatMap{ option =>
				search(newOptions.filter(_ != option), solution :+ option)
			}
		}

		val solutions = search(Range(0, 64).toList, List(0))
//		solutions
//			.foreach { solution =>
//				println(solution.map(intToBinary(_, 6)))
//			}
		println(solutions.maxBy(_.size).map(intToBinary(_, 6)))
		println(solutions.size)
	}

}