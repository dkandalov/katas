package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable


class TruckTour extends Matchers {

	@Test def `hackerrank example`(): Unit = {
		val stations = List(Station(1, 5), Station(10, 3), Station(3, 4))
		findStartStation(stations) shouldEqual 1
	}

	@Test def `long tour`(): Unit = {
		val stations = 0.until(100000).map { _ => Station(0, 1) } :+ Station(200000, 1)
		findStartStation(stations) shouldEqual 100000
	}

	@Test def `determine if tour over stations is valid`(): Unit = {
		isValidTour(List(Station(1, 1)), List()) shouldEqual true
		isValidTour(List(Station(0, 1), Station(1, 1)), List()) shouldEqual false
	}

	def main(args: Array[String]): Unit = {
		val scanner = new java.util.Scanner(System.in)
		val n = scanner.nextLine.toInt

		val stations = 0.until(n).map { _ =>
			val values = scanner.nextLine().split(" ").map(_.toInt)
			Station(values(0), values(1))
		}
		println(findStartStation(stations))
	}

	private def findStartStation(stations: Seq[Station]): Int = {
		val q1 = mutable.Queue[Station](stations: _*)
		val q2 = mutable.Queue[Station]()
		while (q1.nonEmpty) {
			if (isValidTour(q1, q2)) {
				return q2.size
			}
			q2.enqueue(q1.dequeue())
		}
		-1
	}

	private def isValidTour(stations1: Seq[Station], stations2: Seq[Station]): Boolean = {
		var petrol = 0
		stations1.foreach { station =>
			petrol += station.petrol - station.distance
			if (petrol < 0) return false
		}
		stations2.foreach { station =>
			petrol += station.petrol - station.distance
			if (petrol < 0) return false
		}
		true
	}

	private case class Station(petrol: Int, distance: Int)

}