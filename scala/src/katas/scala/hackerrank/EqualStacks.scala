package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

/**
	* https://www.hackerrank.com/challenges/equal-stacks
	*/
class EqualStacks extends Matchers {

	def main(args: Array[String]) {
		val scanner = new java.util.Scanner(System.in)
		val n1 = scanner.nextInt()
		val n2 = scanner.nextInt()
		val n3 = scanner.nextInt()

		val s1 = (for (_ <- 0 until n1) yield scanner.nextInt()).toArray
		val s2 = (for (_ <- 0 until n2) yield scanner.nextInt()).toArray
		val s3 = (for (_ <- 0 until n3) yield scanner.nextInt()).toArray
		
		println(findMinStack(s1, s2, s3))
	}

	def findMinStack(s1: Array[Int], s2: Array[Int], s3: Array[Int]): Int = {
		var s1Sum = s1.sum
		var s2Sum = s2.sum
		var s3Sum = s3.sum
		while (s1Sum != s2Sum || s2Sum != s3Sum) {
			val maxS =
				if (s1Sum >= s2Sum && s1Sum >= s3Sum) s1
				else if (s2Sum >= s1Sum && s2Sum >= s3Sum) s2
				else s3

			var i = 0
			while (i < maxS.length) {
				if (maxS(i) != 0) {
					if (s1Sum >= s2Sum && s1Sum >= s3Sum) s1Sum -= maxS(i)
					else if (s2Sum >= s1Sum && s2Sum >= s3Sum) s2Sum -= maxS(i)
					else s3Sum -= maxS(i)

					maxS(i) = 0
					i = maxS.length
				} else {
					i += 1
				}
			}
		}
		s1.sum
	}

	@Test def `can find min stack height`() {
		findMinStack(Array(), Array(), Array()) shouldEqual 0
		findMinStack(Array(1), Array(), Array()) shouldEqual 0
		findMinStack(Array(1), Array(1), Array()) shouldEqual 0
		findMinStack(Array(1), Array(1), Array(2)) shouldEqual 0
		findMinStack(Array(1), Array(1), Array(1)) shouldEqual 1
		findMinStack(Array(3, 2, 1, 1, 1), Array(4, 3, 2), Array(1, 1, 4, 1)) shouldEqual 5
	}
}