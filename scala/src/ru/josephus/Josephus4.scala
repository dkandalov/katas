package ru.josephus

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * User: DKandalov
 */

class Josephus4 extends AssertionsForJUnit {
  @Test def shouldSolveJosephusProblem() {
    assert(findLeader(1, 1) === 0)
    assert(findLeader(1, 2) === 0)

    assert(findLeader(2, 1) === 1)
    assert(findLeader(2, 2) === 0)
    assert(findLeader(2, 3) === 1)

    assert(findLeader(3, 1) === 2)
    assert(findLeader(3, 2) === 2)
    assert(findLeader(3, 3) === 1)
    assert(findLeader(3, 4) === 1)
    assert(findLeader(3, 5) === 0)
    assert(findLeader(3, 6) === 0)
    assert(findLeader(3, 7) === 2)
    assert(findLeader(3, 8) === 2)

    assert(findLeader(4, 1) === 3)
    assert(findLeader(4, 2) === 0)
    assert(findLeader(4, 3) === 0)
    assert(findLeader(4, 4) === 1)
    assert(findLeader(4, 5) === 1)
    assert(findLeader(4, 6) === 2)
    assert(findLeader(4, 7) === 1)
    assert(findLeader(4, 8) === 2)
    assert(findLeader(4, 9) === 2)
  }

  def findLeader(amountOfPeople: Int, stepSize: Int): Int = {
    var people = for (i <- Range(0, amountOfPeople)) yield i

    var nextPersonToRemove = 0
    while (people.size > 1) {
      nextPersonToRemove += stepSize - 1 // didn't consider that elements are removed when increasing nextPersonToRemove
      if (nextPersonToRemove >= people.size)
        nextPersonToRemove = nextPersonToRemove % people.size // didn't get it right the first time (was in doubt)
      people = people.take(nextPersonToRemove) ++ people.takeRight(people.size - nextPersonToRemove - 1) // was nextPersonToRemove + 1
    }
    people(0)
  }
}