package katas.scala.sieve

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
;

/*
 * User: dima
 * Date: 12/4/11
 * Time: 7:45 PM
 */
class SOE2 extends AssertionsForJUnit {
  @Test def shouldFindPrimeNumbersUpToDefinedLimit() {
    assert(findPrimes(10) === List(1, 2, 3, 5, 7))
    assert(findPrimes(20) === List(1, 2, 3, 5, 7, 11, 13, 17, 19))
    assert(findPrimes(50) === List(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47))
  }

  def findPrimes(n: Int): Seq[Int] = {
    val isPrime = Array.fill(n)(true)
    2.to(n).foreach { i =>
        if (isPrime(i - 1)) { // off-by-one mistake
          (i * 2).to(n, i).foreach { k =>
            isPrime(k - 1) = false // off-by-one mistake
          }
        }
    }
    Range(1, n + 1).zip(isPrime).filter(p => p._2).map(p => p._1)
  }
}