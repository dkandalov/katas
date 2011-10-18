package ru.ladder

import org.junit.Test

 /**
 * User: dima
 * Date: 1/2/11
 */
class Ladder2 {
  @Test
  public void aaa() {
    assert walkDoors(1) == [true]
    assert walkDoors(2) == [true, false]
    assert walkDoors(3) == [true, false, false]
    assert walkDoors(4) == [true, false, false, true]
    assert walkDoors(5) == [true, false, false, true, false]
    assert walkDoors(6) == [true, false, false, true, false, false]
    assert walkDoors(7) == [true, false, false, true, false, false, false]
    assert walkDoors(8) == [true, false, false, true, false, false, false, false]
    assert walkDoors(9) == [true, false, false, true, false, false, false, false, true]
  }

  List walkDoors(int doorsNumber) {
    List doors = (1..doorsNumber).collect {false} // didn't initialize properly

    (1..doorsNumber).each { stepSize ->
      for (int i = stepSize - 1; i < doors.size(); i += stepSize) { // swapped condition/increment operators; incorrect initial i value
        doors[i] = !doors[i]
      }
    }

    doors
  }
}
