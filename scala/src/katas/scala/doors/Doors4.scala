package katas.scala.doors

import org.junit.Test
import org.scalatest.Matchers

/**
 * @author DKandalov
 */
class Doors4 extends Matchers {

  @Test def shouldWalkDoors() {
    walkDoors(2) should equal(List(true, false))
    walkDoors(3) should equal(List(true, false, false))
    walkDoors(4) should equal(List(true, false, false, true))
    walkDoors(5) should equal(List(true, false, false, true, false))
    walkDoors(6) should equal(List(true, false, false, true, false, false))
    walkDoors(7) should equal(List(true, false, false, true, false, false, false))
    walkDoors(8) should equal(List(true, false, false, true, false, false, false, false))
    walkDoors(9) should equal(List(true, false, false, true, false, false, false, false, true))
    walkDoors(10) should equal(List(true, false, false, true, false, false, false, false, true, false))
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

  private def merge(doors1: List[Boolean], doors2: List[Boolean]) =
    doors1.zip(doors2).map { v => v._1 ^ v._2 }
}