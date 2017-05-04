package katas.scala.hackerrank

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
	* https://www.hackerrank.com/challenges/2d-array
	*/
class TwoDArrayDS extends ShouldMatchers {
	def main(args: Array[String]) {
		val sc = new java.util.Scanner(System.in)
		val arr = Array.ofDim[Int](6, 6)
		for (arr_i <- 0 until 6) {
			for (arr_j <- 0 until 6) {
				arr(arr_i)(arr_j) = sc.nextInt()
			}
		}
		println(parseHourGlasses(arr).maxBy(_.sum).sum)
	}

	private def toIntArray(s: String): Array[Array[Int]] = s.split("\n").map{ _.split(" ").map{ _.toInt } }

	private def parseHourGlasses(data: Array[Array[Int]]): Seq[HourGlass] = {
		val xRange = 0 to (data.head.length - 3)
		val yRange = 0 to (data.length - 3)

		yRange.flatMap { y =>
			xRange.map { x =>
				HourGlass(Seq(
					data(y)(x), data(y)(x + 1), data(y)(x + 2),
					data(y + 1)(x + 1),
					data(y + 2)(x), data(y + 2)(x + 1), data(y + 2)(x + 2)
				))
			}
		}
	}

	private case class HourGlass(values: Seq[Int]) {
		val sum: Int = values.sum

		override def toString: String =
			values.take(3).mkString + "\n" +
			" " + values(4) + " \n" +
			values.drop(4).mkString
	}

	@Test def `lists all hour-glasses and find one with max sum`(): Unit = {
		val input = """
      |1 1 1 0 0 0
      |0 1 0 0 0 0
      |1 1 1 0 0 0
      |0 0 2 4 4 0
      |0 0 0 2 0 0
      |0 0 1 2 4 0
    """.trim.stripMargin

		val hourGlasses = parseHourGlasses(toIntArray(input))
		val result = hourGlasses.maxBy(_.sum)

		hourGlasses.take(5).mkString("\n\n") shouldEqual
			"""
				|111
				|_1_
				|111
				|
				|110
				|_1_
				|110
				|
				|100
				|_1_
				|100
				|
				|000
				|_0_
				|000
				|
				|010
				|_0_
				|002
			""".trim.replace('_', ' ').stripMargin

		result shouldEqual HourGlass(Seq(
			2, 4, 4,
			   2,
			1, 2, 4
		))
		result.sum shouldEqual 19
	}
}