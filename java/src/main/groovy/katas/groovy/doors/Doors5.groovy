package katas.groovy.doors

import org.junit.Test

/**
 * User: dima
 * Date: 03/09/2012
 */
class Doors5 {
  @Test void walkingDoors() {
    assert doors(30) ==
            [true, false, false, true, false, false, false, false, true, false, false, false, false, false, false,
                    true, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false]
  }

  static List<Boolean> doors(int size) {
    def doors = (0..size).collect{ false }
    for (int step = 1; step <= size; step++) {
      for (int i = step - 1; i < size; i += step) {
        doors[i] = !doors[i]
      }
    }
    doors
  }
}
