package katas.groovy.doors

import org.junit.Test

class Doors15 {
  private final open = true
  private final closed = false

  @Test void "walking doors"() {
    def amount = 10
    def steps = (1..amount).collectMany{ int stepSize ->
      def steps = []
      (stepSize-1).step(amount, stepSize) { steps << it }
      steps
    }
    def doors = steps.groupBy{it}.sort{it.key}.collect{ it.value.size() % 2 == 1 }

    assert doors == [open, closed, closed, open, closed, closed, closed, closed, open, closed]
  }
}
