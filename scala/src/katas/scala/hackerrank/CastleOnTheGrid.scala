package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

		findPath(grid, Point(0, 0), Point(0, 2)) shouldEqual List(Point(0, 0), Point(1, 0), Point(1, 2), Point(0, 2))
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

	@Test def `large grid`(): Unit = {
		val grid = Grid(Array.fill(100){ Array.fill[Char](100){ '.' } })
		findPath(grid, Point(0, 0), Point(99, 99)) shouldEqual List(
			Point(0, 0), Point(0, 99), Point(99, 99)
		)
	}
	
	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)

		val n = scanner.nextLine().toInt
		val grid = Grid(0.until(n).map { _ => scanner.nextLine().toCharArray }.toArray)

		val a = scanner.nextLine().split(" ").map(_.toInt)
		val from = Point(a(1), a(0))
		val to = Point(a(3), a(2))

		val path = findPath(grid, from, to)

		println(path.size - 1)
	}

	import Grid._

	private def findPath(grid: Grid, from: Point, to: Point): List[Point] = {
		val queue = mutable.Queue(ListBuffer(from))
		while (queue.nonEmpty && queue.head.last != to) {
			val path = queue.dequeue()
			val moves = validMoves(grid, path.last)
			moves.foreach{ move =>
				if (grid(move) == emptyCell) {
					queue.enqueue(path :+ move)
					grid(move) = visitedCell
				}
			}
		}
		queue.headOption.map(_.toList).orNull
	}

	private def validMoves(grid: Grid, p: Point): List[Point] = {
		val width = grid.size
		val height = width

		val result = new ListBuffer[Point]()
		var dp = p.copy(y = p.y + 1)
		while (dp.y < height && grid(dp) != cellWithWall) {
			result += dp
			dp = dp.copy(y = dp.y + 1)
		}
		dp = p.copy(y = p.y - 1)
		while (dp.y >= 0 && grid(dp) != cellWithWall) {
			result += dp
			dp = dp.copy(y = dp.y - 1)
		}
		dp = p.copy(x = p.x + 1)
		while (dp.x < width && grid(dp) != cellWithWall) {
			result += dp
			dp = dp.copy(x = dp.x + 1)
		}
		dp = p.copy(x = p.x - 1)
		while (dp.x >= 0 && grid(dp) != cellWithWall) {
			result += dp
			dp = dp.copy(x = dp.x - 1)
		}
		result.toList
	}

	private case class Point(x: Int, y: Int) {
		override def toString: String = s"[$x,$y]"
	}

	private case class Grid(private val data: Array[Array[Char]]) {
		val size: Int = data.length
		def apply(p: Point): Char = data(p.y)(p.x)
		def update(p: Point, c: Char): Unit = data(p.y)(p.x) = c
	}

	private object Grid {
		val emptyCell: Char = '.'
		val cellWithWall: Char = 'X'
		val visitedCell: Char = '*'
	}

	private implicit class StringToGrid(s: String) {
		def parse(): Grid = Grid(s.split("\n").map(_.toCharArray))
	}
}