package ru.doors

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import java.util.Arrays

/**
 * User: DKandalov
 */

class Doors2 extends AssertionsForJUnit
{
  @Test
  def shouldWalkDoors()
  {
    assert(List(false) === List(false))
    assert(Arrays.equals(walkDoors(1), Array(false))) // forgot to change List to Array // Array(false) did not equal Array(false)
    assert(Arrays.equals(walkDoors(2), Array(false, true)))
    assert(Arrays.equals(walkDoors(3), Array(false, true, true)))
    assert(Arrays.equals(walkDoors(4), Array(false, true, true, false)))
    assert(Arrays.equals(walkDoors(5), Array(false, true, true, false, true)))
    assert(Arrays.equals(walkDoors(6), Array(false, true, true, false, true, true)))
    assert(Arrays.equals(walkDoors(7), Array(false, true, true, false, true, true, true)))
  }

  def walkDoors(doorsAmount: Int): Array[Boolean] =
  {
    val doors = new Array[Boolean](doorsAmount)
    for (i <- 1.to(doorsAmount)) {
      doors(i - 1) = true // array index out of bound
    }

    for (stepSize <- 1 to doorsAmount) {
      //      for (i <- (stepSize - 1).to(doorsAmount, stepSize)) {
      var i = stepSize - 1
      while (i < doorsAmount) {
        doors(i) = !doors(i)
        i = i + stepSize // used increment right after "while" what cause out of bound exception
      }
    }

    doors
  }
}