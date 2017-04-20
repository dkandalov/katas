package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable

/**
	* https://www.hackerrank.com/challenges/castle-on-the-grid
	*/
class CastleOnTheGrid extends Matchers {

	@Test def `linear downward path`(): Unit = {
		val grid =
			"""
				|...
				|...
				|...
			""".trim.stripMargin.parse()

		findPath(grid, Point(0, 0), Point(0, 2)) shouldEqual List(Point(0, 0), Point(0, 2))
	}

	@Test def `downward path with obstacle`(): Unit = {
		val grid =
			"""
				|...
				|X..
				|...
			""".trim.stripMargin.parse()

		findPath(grid, Point(0, 0), Point(0, 2)) shouldEqual List(Point(0, 0), Point(2, 0), Point(2, 2), Point(0, 2))
	}

	@Test def `downward path with obstacle 2`(): Unit = {
		val grid =
			"""
				|...
				|X.X
				|...
			""".trim.stripMargin.parse()

		findPath(grid, Point(0, 0), Point(0, 2)) shouldEqual List(Point(0, 0), Point(1, 0), Point(1, 2), Point(0, 2))
	}

	@Test def `upward path with obstacle 2`(): Unit = {
		val grid =
			"""
				|...
				|X.X
				|...
			""".trim.stripMargin.parse()

		findPath(grid, Point(2, 2), Point(0, 0)) shouldEqual List(Point(2, 2), Point(1, 2), Point(1, 0), Point(0, 0))
	}

	@Test def `no path`(): Unit = {
		val grid =
			"""
				|...
				|XXX
				|...
			""".trim.stripMargin.parse()

		findPath(grid, Point(0, 0), Point(2, 2)) shouldEqual null
	}
	
	@Test def `hackerrank example`(): Unit = {
		val grid =
			"""
				|.X.
				|.X.
				|...
			""".trim.stripMargin.parse()

		findPath(grid, Point(0, 0), Point(2, 0)) shouldEqual List(Point(0, 0), Point(0, 2), Point(2, 2), Point(2, 0))
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)

		val n = scanner.nextInt()
		val grid = 0.until(n).map { _ => scanner.nextLine().toCharArray }.toArray

		val fromY = scanner.nextInt
		val fromX = scanner.nextInt
		val toY = scanner.nextInt
		val toX = scanner.nextInt

		val path = findPath(grid, Point(fromX, fromY), Point(toX, toY))

		println(path.size)
	}

	private def findPath(grid: Array[Array[Char]], from: Point, to: Point): List[Point] = {
		val queue = mutable.Queue(List(from))
		while (queue.nonEmpty && queue.head.last != to) {
			val path = queue.dequeue()
			val moves = validMoves(grid, path.last)
			moves.foreach{ move =>
				if (!path.contains(move)) {
					queue.enqueue(path :+ move)
				}
			}
		}
		queue.headOption.orNull
	}

	private def validMoves(grid: Array[Array[Char]], p: Point): List[Point] = {
		val width = grid.length
		val height = width

		val result = new mutable.ListBuffer[Point]()
		var dy = 1
		while (p.y + dy < height && grid(p.y + dy)(p.x) != 'X') {
			result += Point(p.x, p.y + dy)
			dy += 1
		}
		dy = -1
		while (p.y + dy >= 0 && grid(p.y + dy)(p.x) != 'X') {
			result += Point(p.x, p.y + dy)
			dy -= 1
		}
		var dx = 1
		while (p.x + dx < width && grid(p.y)(p.x + dx) != 'X') {
			result += Point(p.x + dx, p.y)
			dx += 1
		}
		dx = -1
		while (p.x + dx >= 0 && grid(p.y)(p.x + dx) != 'X') {
			result += Point(p.x + dx, p.y)
			dx -= 1
		}
		result.toList.reverse
	}

	private case class Point(x: Int, y: Int) {
		override def toString: String = s"[$x,$y]"
	}

	private implicit class StringToGrid(s: String) {
		def parse(): Array[Array[Char]] = s.split("\n").map(_.toCharArray)
	}
}