package ru.josephus

/**
 * User: DKandalov
 */

object Josephus1
{
  def main(args: Array[String])
  {
    assert(findLeader(1, 1) == 1)
    assert(findLeader(1, 2) == 1)
    assert(findLeader(1, 3) == 1)

    assert(findLeader(2, 1) == 2)
    assert(findLeader(2, 2) == 1)
    assert(findLeader(2, 3) == 2)
    assert(findLeader(2, 4) == 1)

    assert(findLeader(3, 1) == 3)
    assert(findLeader(3, 2) == 3)
    assert(findLeader(3, 3) == 2)
    assert(findLeader(3, 4) == 2)
    assert(findLeader(3, 5) == 1)
    assert(findLeader(3, 6) == 1)
  }

  def findLeader(size: Int, step: Int): Int =
  {
//    var list: List[Int] = List()
//    size.to(1).foreach((i:Int) =>
//      list = i :: list
//    )
//    println list
    1
  }
}