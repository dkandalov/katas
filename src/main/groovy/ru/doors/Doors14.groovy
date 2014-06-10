package ru.doors

import org.junit.Test

class Doors14 {
  @Test void "walking doors"() {
    def amount = 10
    def steps = (1..10).collectMany { int stepSize ->
      (1..(amount.intdiv(stepSize))).collect{ stepSize * it - 1 }
    }
    def doors = steps.groupBy{it}.entrySet().sort{it.key}.collect{ it.value.size() % 2 == 1 }

    assert doors == [true, false, false, true, false, false, false, false, true, false]
  }
}
