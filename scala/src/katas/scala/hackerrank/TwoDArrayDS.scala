package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

/**
	* https://www.hackerrank.com/challenges/2d-array
	*/
class TwoDArrayDS extends Matchers {
	def main(args: Array[String]) {
		val sc = new java.util.Scanner(System.in)
		val arr = Array.ofDim[Int](6, 6)
		for (arr_i <- 0 until 6) {
			for (arr_j <- 0 until 6) {
				arr(arr_i)(arr_j) = sc.nextInt()
			}
		}
	}

	@Test def `lists all hour-glasses`(): Unit = {
		val input = """
				|1 1 1 0 0 0
				|0 1 0 0 0 0
				|1 1 1 0 0 0
				|0 0 2 4 4 0
				|0 0 0 2 0 0
				|0 0 1 2 4 0
			""".trim.stripMargin

		val lines: Array[Array[Int]] = input.split("\n").map{ _.split(" ").map{ _.toInt } }
		lines.foreach{ _.toList }

		val xRange = 0 to (lines.head.length - 3)
		val yRange = 0 to (lines.length - 3)
		println(xRange)
		println(yRange)

		yRange.foreach { y =>
			xRange.foreach { x =>
				val hourGlass = "" +
					lines(y)(x) + lines(y)(x + 1) + lines(y)(x + 2) + "\n" +
					" " + lines(y + 1)(x + 1) + " \n" +
					lines(y + 2)(x) + lines(y + 2)(x + 1) + lines(y + 2)(x + 2) + "\n"
				println(hourGlass)
			}
		}
	}
}