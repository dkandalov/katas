package ru.josephus

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * User: DKandalov
 */

class Josephus1 extends AssertionsForJUnit
{
  @Test
  def shouldFindLeader()
  {
    assert(findLeader(1, 1) == 1)
    assert(findLeader(1, 2) == 1)
    assert(findLeader(1, 3) == 1)

    assert(findLeader(2, 1) == 2)
    assert(findLeader(2, 2) == 1)
    assert(findLeader(2, 3) == 2)
    assert(findLeader(2, 4) == 1)

    assert(findLeader(3, 1) == 3)
    assert(findLeader(3, 2) == 3)
    assert(findLeader(3, 3) == 2)
    assert(findLeader(3, 4) == 2)
    assert(findLeader(3, 5) == 1)
    assert(findLeader(3, 6) == 1)
  }

  def findLeader(size: Int, step: Int): Int =
  {
    var list: List[Int] = List()
    1.to(size).foreach((i: Int) => list = i :: list)
    list = list.reverse

    findLeader(list, step)
  }

  def findLeader(list: List[Int], step: Int): Int =
  {
    list match {
      case List(x) => x
      case List(x, _*) =>
        val actualStep = (step - 1) % list.size
        val tuple = list.splitAt(actualStep)
        val part2 = if (tuple._2.isEmpty) List() else tuple._2.tail
        findLeader(part2 ::: tuple._1, step) // "tail" doesn't work on empty lists
    }
  }
}