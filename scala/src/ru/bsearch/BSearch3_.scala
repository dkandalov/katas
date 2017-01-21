package ru.bsearch

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * User: DKandalov
 */
class BSearch3_ extends AssertionsForJUnit {
  @Test def shouldFindValueInASortedList() {
    assert(find(0, List()) === -1)

    assert(find(0, List(1)) === -1)
    assert(find(1, List(1)) === 0)
    assert(find(2, List(1)) === -1)

    assert(find(0, List(1, 2)) === -1)
    assert(find(1, List(1, 2)) === 0)
    assert(find(2, List(1, 2)) === 1)
    assert(find(3, List(1, 2)) === -1)

    assert(find(0, List(1, 2, 3)) === -1)
    assert(find(1, List(1, 2, 3)) === 0)
    assert(find(2, List(1, 2, 3)) === 1)
    assert(find(3, List(1, 2, 3)) === 2)
    assert(find(4, List(1, 2, 3)) === -1)

    assert(find("1", List("0", "a", "z")) === -1)
    assert(find("0", List("0", "a", "z")) === 0)
    assert(find("a", List("0", "a", "z")) === 1)
    assert(find("z", List("0", "a", "z")) === 2)
  }

  def find[T](value: T, list: List[T], shift: Int = 0)(implicit orderer: T => Ordered[T]): Int = {
    if (list.isEmpty) return -1

    val midPos = list.size / 2
    if (value == list(midPos))
      shift + midPos
    else if (value < list(midPos))
      find(value, list.take(midPos), shift)
    else // value > midValue
      find(value, list.drop(midPos + 1), shift + midPos + 1) // didn't exclude midPos values, made quite a few attempts to understand what's wrong
  }
}