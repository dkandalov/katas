package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class DynamicArray extends Matchers {
	@Test def `the silly task`(): Unit = {
		val input = """
			|2 5
			|1 0 5
			|1 1 7
			|1 0 3
			|2 1 0
			|2 1 1
      """.trim.stripMargin.split("\n")
		val expectedOutput = """
			|7
			|3
			""".trim.stripMargin + "\n"

		var actualOutput = ""
		process(input.toIterator, { actualOutput += _ + "\n" })
		actualOutput should equal(expectedOutput)
	}

	private def solution(): Unit = {
		process(Source.stdin.getLines, { println(_) })
	}

	private def process(input: Iterator[String], callback: (Int) => Unit) = {
		val header = input.next().split(" ").map(_.toInt)
		val n = header(0)
		val q = header(1)

		val seqList = 0.until(n).map { _ => new ArrayBuffer[Int]() }
		var lastAns = 0

		input.foreach { line =>
			val values = line.split(" ").map(_.toInt)
			val queryType = values(0)
			val x = values(1)
			val y = values(2)

			val list = seqList((x ^ lastAns) % n)
			if (queryType == 1) {
				list.append(y)
			} else if (queryType == 2) {
				lastAns = list(y % list.size)
				callback(lastAns)
			}
		}
	}

}