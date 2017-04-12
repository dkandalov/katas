package katas.groovy.josephus

import org.junit.Test

/**
 * User: dima
 * Date: 04/01/2012
 */
class JP6 {
  @Test
  public void shouldFindLeader() {
    assert findLeader(1, 1) == 1
    assert findLeader(1, 2) == 1

    assert findLeader(2, 1) == 2
    assert findLeader(2, 2) == 1
    assert findLeader(2, 3) == 2
    assert findLeader(2, 4) == 1

    assert findLeader(3, 1) == 3
    assert findLeader(3, 2) == 3
    assert findLeader(3, 3) == 2
    assert findLeader(3, 4) == 2
    assert findLeader(3, 5) == 1
    assert findLeader(3, 6) == 1
    assert findLeader(3, 7) == 3
    assert findLeader(3, 8) == 3
    assert findLeader(3, 9) == 2

    assert findLeader(9, 1) == 9
    assert findLeader(9, 2) == 3
    assert findLeader(9, 3) == 1
    assert findLeader(9, 5) == 8
  }

  def findLeader(int numberOfPeople, int stepSize) {
    def people = (1..numberOfPeople).toList()
    int nextPerson = 0
    (numberOfPeople - 1).times {
      nextPerson = (nextPerson + stepSize - 1) % people.size()
      people.remove((int) nextPerson)
    }
    people[0]
  }

  // quick coding
  def findLeader_(int numberOfPeople, int stepSize) {
    def people = (1..numberOfPeople).toList()
    int nextPerson = 0
    while (people.size() > 1) {
      nextPerson += stepSize - 1
      if (nextPerson >= people.size()) nextPerson = nextPerson % people.size()
      people.remove((int) nextPerson)
    }
    people[0]
  }
}
