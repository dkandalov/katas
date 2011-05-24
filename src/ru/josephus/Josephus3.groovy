package ru.josephus

import org.junit.Test

/**
 * User: DKandalov
 */
class Josephus3 {
  @Test
  public void shouldFindALeader() {
    assert findLeader(1, 1) == 0

    assert findLeader(2, 1) == 1
    assert findLeader(2, 2) == 0
    assert findLeader(2, 3) == 1
    assert findLeader(2, 4) == 0

    assert findLeader(3, 1) == 2
    assert findLeader(3, 2) == 2
    assert findLeader(3, 3) == 1
    assert findLeader(3, 4) == 1
    assert findLeader(3, 5) == 0
    assert findLeader(3, 6) == 0
    assert findLeader(3, 7) == 2
    assert findLeader(3, 8) == 2

    assert findLeader(4, 1) == 3
    assert findLeader(4, 2) == 0
    assert findLeader(4, 3) == 0
    assert findLeader(4, 4) == 1
    assert findLeader(4, 5) == 1
    assert findLeader(4, 6) == 2
    assert findLeader(4, 7) == 1
    assert findLeader(4, 8) == 2
    assert findLeader(4, 9) == 2
  }

  def findLeader(int amountOfPeople, int stepSize) {
    def people = (0..amountOfPeople - 1).toList()
    def position = -1 // didn't have position
    while (people.size() > 1) {
      position += stepSize
      position = (position >= people.size() ? position % people.size() : position) // forgot this condition, got it wrong first two times
      people.remove(position)
      position-- // didn't have decrement
    }
    people[0]
  }
}
