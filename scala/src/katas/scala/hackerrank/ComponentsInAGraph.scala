package katas.scala.hackerrank

import scala.collection.mutable.ArrayBuffer


class ComponentsInAGraph {
	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)

		val board = new Board(15000)
		val n = scanner.nextLine().toInt
		0.until(n)
			.map { _ => scanner.nextLine().split(" +") }
			.map { values => (values(0).toInt, values(1).toInt) }
			.foreach{ t => board.connect(t._1, t._2) }

		val grouped = board.data.zipWithIndex.groupBy{ _._1 }.filter{ _._2.size > 1 }
		print(grouped.maxBy(_._2.size)._2.size + " ")
		print(grouped.minBy(_._2.size)._2.size + " ")
	}

	class Board(size: Int) {
		val data: ArrayBuffer[Int] = new ArrayBuffer[Int]()
		data.insertAll(0, Range(0, size))

		def connected(p1: Int, p2: Int): Boolean = {
			data(p1) == data(p2)
		}

		def connect(p1: Int, p2: Int) {
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