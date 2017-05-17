package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable
import scala.util.Random


class ComponentsInAGraph extends Matchers {

	@Test def `determine if components are connected`(): Unit = {
		val board = new Board()
		board.areConnected(0, 1) should equal(false)
		board.connect(0, 1)
		board.areConnected(0, 1) should equal(true)
	}

	@Test def `hackerrank example`(): Unit = {
		val board = new Board()
		board.connect(1, 6)
		board.connect(2, 7)
		board.connect(3, 8)
		board.connect(4, 9)
		board.connect(2, 6)
		board.minMaxComponentsSize() should equal((2, 4))
	}

	@Test def `hackerrank timing out example`(): Unit = {
		val board = new Board()
		15000.to(30000).foreach {
			board.connect(1, _)
		}
	}

	@Test def `inserting random elements into large board`(): Unit = {
		val seed = new Random().nextLong()
		println(s"seed = $seed")
		val random = new Random(seed)

		val board = new Board()
		val n = 1000
		0.until(n).foreach{ _ =>
			val p1 = random.nextInt(n)
			val p2 = random.nextInt(n)
			board.connect(p1, p2)
		}
		println(board.minMaxComponentsSize())
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)

		val board = new Board()
		val n = scanner.nextLine().toInt
		0.until(n)
			.map { _ => scanner.nextLine().split(" +") }
			.map { values => (values(0).toInt, values(1).toInt) }
			.foreach{ t => board.connect(t._1, t._2) }

		val (min, max) = board.minMaxComponentsSize()
		println(min + " " + max)
	}

	class Board() {
		private val map = new mutable.HashMap[Int, Int]()

		def minMaxComponentsSize(): (Int, Int) = {
			val countByRoot = new mutable.HashMap[Int, Int]().withDefaultValue(0)
			map.foreach{ entry =>
				countByRoot(rootOf(entry._1)) += 1
			}
			(countByRoot.minBy(_._2)._2, countByRoot.maxBy(_._2)._2)
		}

		def areConnected(p1: Int, p2: Int): Boolean = {
			if (!map.contains(p1)) false
			else if (!map.contains(p2)) false
			else {
				val p1Root = rootOf(p1)
				val p2Root = rootOf(p2)
				p1Root == p2Root
			}
		}

		def connect(p1: Int, p2: Int) {
			if (!map.contains(p1)) map.put(p1, p1)
			if (!map.contains(p2)) {
				map.put(p2, p1)
				return
			}

			val p1Root = rootOf(p1)
			val p2Root = rootOf(p2)

			map(p1Root) = p2Root
		}

		private def rootOf(p: Int): Int = {
			var root = map(p)
			while (map(root) != root) {
				root = map(root)
			}
			root
		}
	}
}