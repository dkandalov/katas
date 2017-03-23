package katas.scala.permutation

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * User: dima
 * Date: 8/2/11
 */

class Perm7 extends AssertionsForJUnit {
  @Test def aaa() {
    assert(perm(List()) === List(List()))
    assert(perm(List(1)) === List(List(1)))
    assert(perm(List(1, 2)) === List(List(1, 2), List(2, 1))) // was wrong test
    assert(perm(List(1, 2, 3)) === List(
      List(1, 2, 3), List(1, 3, 2),
      List(2, 1, 3), List(2, 3, 1),
      List(3, 1, 2), List(3, 2, 1)
    ))
  }

  def perm(list: List[Int]): List[List[Int]] = {
    if (list.isEmpty) return List(List())
    if (list.size == 1) return List(list)

    0.until(list.size).foldRight(List[List[Int]]()) { (i,a) =>
        val splitted = list.splitAt(i)
//        println(i)
//        println(splitted)
        perm(splitted._1 ::: splitted._2.tail).map { subResult: List[Int] =>
            splitted._2.head :: subResult
        }.foldRight(a) { _ :: _ } // used :: instead of :::. Note that :: is like add(), ::: is like addAll()
    }
  }
}