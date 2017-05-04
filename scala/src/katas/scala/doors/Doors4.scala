package katas.scala.doors

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

/**
 * @author DKandalov
 */
class Doors4 extends ShouldMatchers {

  @Test def shouldWalkDoors() {
    walkDoors(2) should equalTo(List(true, false))
    walkDoors(3) should equalTo(List(true, false, false))
    walkDoors(4) should equalTo(List(true, false, false, true))
    walkDoors(5) should equalTo(List(true, false, false, true, false))
    walkDoors(6) should equalTo(List(true, false, false, true, false, false))
    walkDoors(7) should equalTo(List(true, false, false, true, false, false, false))
    walkDoors(8) should equalTo(List(true, false, false, true, false, false, false, false))
    walkDoors(9) should equalTo(List(true, false, false, true, false, false, false, false, true))
    walkDoors(10) should equalTo(List(true, false, false, true, false, false, false, false, true, false))
  }

  def walkDoors_(doorsSize: Int): List[Boolean] = {
    val doors = Array.fill(doorsSize) {false} // used def instead of val

    Range(1, doorsSize + 1).foreach { stepSize => // mistakes with range
      Range(-1, doorsSize, stepSize).foreach { position =>
        if (position != -1) doors(position) = !doors(position)
      }
    }
    doors.toList
  }

  def walkDoors(doorsSize: Int, stepSize: Int = 1): List[Boolean] = {
    val doors = Range(0, doorsSize).map { i => (i + 1) % stepSize == 0 }.toList // didn't add parenthesis to (i + 1)
    if (stepSize < doorsSize) merge(doors, walkDoors(doorsSize, stepSize + 1)) else doors
  }

  def merge(doors1: List[Boolean], doors2: List[Boolean]) =
    doors1.zip(doors2).map{ v => v._1 ^ v._2 }.toList
}