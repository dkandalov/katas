package katas.scala.hackerrank

import org.junit.Test
import org.scalatest.Matchers

import scala.collection.mutable


class TruckTour extends Matchers {

	@Test def `hackerrank example`(): Unit = {
		val stations = List(Station(1, 5), Station(10, 3), Station(3, 4))
		findStartStation(0, stations) shouldBe 1
	}

		def main(args: Array[String]): Unit = {
			val scanner = new java.util.Scanner(System.in)
			val n = scanner.nextLine.toInt

			val stations = 0.until(n).map { _ =>
				val values = scanner.nextLine().split(" ").map(_.toInt)
				Station(values(0), values(1))
			}
			println(findStartStation(0, stations))
		}

		private def findStartStation(truckPetrol: Int, stations: Seq[Station]): Int = {
			val q1 = mutable.Queue[Station](stations: _*)
			val q2 = mutable.Queue[Station]()
			while (q1.nonEmpty) {
				val tour = q1 ++ q2
				if (isValid(truckPetrol, tour)) {
					return q2.size
				}
				q2.enqueue(q1.dequeue())
			}
			-1
		}

		private def isValid(truckPetrol: Int, tour: Seq[Station]): Boolean = {
			var petrol = truckPetrol
			tour.foreach { station =>
				petrol += station.petrol - station.distance
				if (petrol < 0) return false
			}
			true
		}

		private case class Station(petrol: Int, distance: Int)

}