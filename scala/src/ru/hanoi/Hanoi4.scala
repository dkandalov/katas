package ru.hanoi

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
;

/*
 * User: dima
 * Date: 9/4/11
 * Time: 2:21 PM
 */
class Hanoi4 extends AssertionsForJUnit {
  @Test def shouldSolveHanoi() {
    assert(solveHanoi(1) === List(Move(1, 1)))
    assert(solveHanoi(2) === List(Move(1, -1), Move(2, 1), Move(1, -1)))
    assert(solveHanoi(3) === List(Move(1, 1), Move(2, -1), Move(1, 1), Move(3, 1), Move(1, 1), Move(2, -1), Move(1, 1)))
  }

  case class Move(disk: Int, direction: Int)

  def solveHanoi(hanoiSize: Int, direction: Int = 1): List[Move] = {
    if (hanoiSize == 0) // forgot to add this condition
      List()
    else
      solveHanoi(hanoiSize - 1, -direction) :::
        List(Move(hanoiSize, direction)) ::: // tried to append to just one element not a List
        solveHanoi(hanoiSize - 1, -direction)
  }
}