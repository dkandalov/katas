package ru.doors

import org.junit.Test

class Doors15 {
  @Test void "walking doors"() {
    def amount = 10
    def steps = (1..amount).collectMany{ int stepSize ->
      def steps = []
      (stepSize-1).step(amount, stepSize) { steps << it }
      steps
    }
    def doors = steps.groupBy{it}.collect{ it.value.size() % 2 == 1}

    assert doors == [true, false, false, true, false, false, false, false, true, false]
  }
}
