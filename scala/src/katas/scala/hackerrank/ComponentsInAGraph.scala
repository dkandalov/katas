package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class ComponentsInAGraph extends Matchers {
	@Test def `determine if components are connected`(): Unit = {
		val board = new Board(10)
		board.areConnected(0, 1) should equal(false)
		board.connect(0, 1)
		board.areConnected(0, 1) should equal(true)
	}

	@Test def `hackerrank example`(): Unit = {
		val board = new Board(10)
		board.connect(1, 6)
		board.connect(2, 7)
		board.connect(3, 8)
		board.connect(4, 9)
		board.connect(2, 6)
		board.minMaxComponentsSize() should equal((2, 4))
	}

	@Test def `inserting random elements into large board`(): Unit = {
		val seed = new Random().nextLong()
		println(s"seed = $seed")
		val random = new Random(seed)

		val board = new Board(1000)
		0.until(board.size).foreach{ _ =>
			val p1 = random.nextInt(board.size)
			val p2 = random.nextInt(board.size)
			board.connect(p1, p2)
		}
		println(board.minMaxComponentsSize())
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)

		val board = new Board(25000)
		val n = scanner.nextLine().toInt
		0.until(n)
			.map { _ => scanner.nextLine().split(" +") }
			.map { values => (values(0).toInt, values(1).toInt) }
			.foreach{ t => board.connect(t._1, t._2) }

		val (min, max) = board.minMaxComponentsSize()
		println(min + " " + max)
	}

	class Board(val size: Int) {
		private val data: ArrayBuffer[Int] = new ArrayBuffer[Int]()
		data.insertAll(0, Range(0, size))

		def areConnected(p1: Int, p2: Int): Boolean = {
			data(p1) == data(p2)
		}

		def minMaxComponentsSize(): (Int, Int) = {
			val grouped = data.zipWithIndex
				.groupBy{ _._1 }
				.map{ it => (it._1, it._2.size) }
				.filter{ _._2 > 1 }
			(grouped.minBy(_._2)._2, grouped.maxBy(_._2)._2)
		}

		def connect(p1: Int, p2: Int) {
			if (p1 == p2) return

			val p1Root = rootOf(p1, data)
			val p2Root = rootOf(p2, data)

			data.indices
				.filter {data(_) == p1Root}
				.foreach {data(_) = p2Root}
		}

		private def rootOf(p: Int, data: ArrayBuffer[Int]): Int = {
			var root = data(p)
			while (data(root) != root) {
				root = data(root)
			}
			root
		}
	}

}