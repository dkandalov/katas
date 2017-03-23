package katas.scala.fibonacci

/**
 * User: dima
 * Date: 6/2/11
 */

object Fibonacci4 {
  def main(args: Array[String]) {
    println(fibonacci(-2)) // -1
    println(fibonacci(-1)) // -1
    println(fibonacci(0)) // 0
    println(fibonacci(1)) // 1
    println(fibonacci(2)) // 1
    println(fibonacci(3)) // 2
    println(fibonacci(4)) // 3
    println(fibonacci(5)) // 5
    println(fibonacci(6)) // 8
    println(fibonacci(7)) // 13
  }

  def fibonacci_(v: Int): Int = {
    if (v == -1) return -1

    var result = 0
    var prevValue = 1
    1.to(v).foreach { i:Int =>
      val tmp = result
      result += prevValue
      prevValue = tmp
    }
    result
  }

  def fibonacci(v: Int): Int = v match {
    case (-1 | 0 | 1) => return v
    case n => fibonacci(n - 2) + fibonacci(n - 1)
  }
}