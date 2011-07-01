package ru.misc

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.math.BigDecimal.int2bigDecimal
import scala.math._

/**
 * User: DKandalov
 */
class Ackermann0 extends AssertionsForJUnit {
  @Test def ackermannsFunction() {
    assert(f(0, 0) === 0)
    assert(f(0, 1) === 2)
    assert(f(0, 2) === 2 * 2)
    assert(f(0, 10) === 2 * 10)

    //assert(f(1, 0) === scala.math.pow(2, 0))
    assert(f(1, 1) === pow(2, 1))
    assert(f(1, 2) === pow(2, 2))
    assert(f(1, 5) === pow(2, 5))
    assert(f(1, 10) === pow(2, 10))

    assert(f(2, 1) === pow(2, pow(2, 0)))
    assert(f(2, 2) === pow(2, pow(2, 1)))
    assert(f(2, 3) === pow(2, pow(2, 2)))
    assert(f(2, 4) === pow(2, pow(2, 4)))
//    assert(f(2, 5) === 0) // never finishes, is it pow(2, pow(2, 8)) ?

    assert(f(3, 3) === 65536)
  }

  private def f(x: BigDecimal, y: BigDecimal): BigDecimal = {
    if (y == 0) return 0
    if (y == 1) return 2
    if (x == 0) return 2 * y
    f(x - 1, f(x, y - 1))
  }

  /*
f(2 3)
f(1 f(2 2))
f(1 f(1 f(2, 1))
f(1 f(1 2))
f(1 f(0 f(1, 1))
f(1 f(0 2))
f(1 4)
f(0 f(1, 3))
f(0 f(0, f(1, 2)))
f(0 f(0, f(0, f(1, 1))))
f(0 f(0, f(0, 2)))
f(0 f(0, 4))
f(0 8)
16
  */
}