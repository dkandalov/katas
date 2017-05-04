package katas.scala.sieve

import org.junit.Test
import org.specs2.matcher.ShouldMatchers

class Sieve0 extends ShouldMatchers {

  @Test def shouldFindPrimeNumbers() {
    findPrimesUpTo(10) should equalTo(List(2, 3, 5, 7))
    findPrimesUpTo(20) should equalTo(List(2, 3, 5, 7, 11, 13, 17, 19))
    findPrimesUpTo(99) should equalTo(List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97))
  }

  def findPrimesUpTo(n: Int): List[Int] = {
    if (n < 2) return List()

    val primes = findPrimesUpTo(n - 1)
    if (primes.exists(n % _ == 0)) return primes
    primes ::: List(n) // didn't find a better way to "reverse" :(
  }

  def findPrimesUpTo_it(n: Int): List[Int] = {
    var primes = List[Int]()
    2.to(n).foreach { i =>
      if (!primes.exists(i % _ == 0)) { // divided wrong way "_ % i == 0"
        primes = i :: primes
      }
    }
    primes.reverse
  }
}