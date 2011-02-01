package ru.fibonacci

/**
 * User: DKandalov
 */

object Fibonacci1
{
  def main(args: Array[String])
  {
    def fibonacci(i: Int): Int = i match
    {
      case 0 => 0
      case 1 => 1
      case n => fibonacci(i - 2) + fibonacci(i - 1)
    }

    0.to(7).foreach((i: Int) =>
      println(fibonacci(i))
    )
  }
}