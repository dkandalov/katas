package katas.scala.josephus

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/*
 * User: dima
 * Date: 1/3/11
 * Time: 8:06 AM
 */
class JP4 extends AssertionsForJUnit {
  @Test def shouldFindLeader() {
    assert(findLeader(1, 1) === 1)
    assert(findLeader(1, 2) === 1)

    assert(findLeader(2, 1) === 2)
    assert(findLeader(2, 2) === 1)
    assert(findLeader(2, 3) === 2)
    assert(findLeader(2, 4) === 1)

    assert(findLeader(3, 1) === 3)
    assert(findLeader(3, 2) === 3)
    assert(findLeader(3, 3) === 2)
    assert(findLeader(3, 4) === 2)
    assert(findLeader(3, 5) === 1)
  }

  def findLeader(amountOfPeople: Int, stepSize: Int): Int = {
    def doFindLeader(people: List[Int]): Int = {
      if (people.size == 1) return people(0)

      val actualStepSize = ((stepSize - 1) % people.size) + 1
      val tmp = people.splitAt(actualStepSize)
      doFindLeader(tmp._2 ::: tmp._1.dropRight(1))
    }

    var i: Int = 0
    val people = List.fill(amountOfPeople) {
      i = i + 1;
      i
    }
    doFindLeader(people)
  }
}