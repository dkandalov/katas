package ru.doors

import collection.mutable.ListBuffer

/**
 * User: DKandalov
 */

object Doors0
{
  def main(args: Array[String])
  {
    1.to(12).foreach((i: Int) =>
      println(walkDoors(i))
    )
  }

  def walkDoors(i: Int): ListBuffer[Boolean] =
  {
    val result = ListBuffer.fill(i) {false}

    1.to(i).foreach( stepSize => {
      var j = stepSize - 1
      while (j < result.length) {
        result(j) = !result(j)
        j = j + stepSize
      }
    })

    result
  }
}