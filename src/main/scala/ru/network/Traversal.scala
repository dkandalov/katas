package ru.network

import scala.actors.Actor
import scala.actors.Actor._

/**
 * @author DKandalov
 */

object Traversal {
  def main(args: Array[String]) {
    val tree = Tree(0,
      Tree(1),
      Tree(2)
    )
    new PreOrder(tree).iterator.foreach(println)
  }
}

case class Tree(elem: Int, left: Tree = null, right: Tree = null)

case object Next
case object Stop
case object Undefined

class PreOrder(n: Tree) extends Producer[Int] {
  def produceValues() { traverse(n) }

  def traverse(n: Tree) {
    if (n != null) {
      produce(n.elem)
      traverse(n.left)
      traverse(n.right)
    }
  }
}

abstract class Producer[T] {
  protected def produceValues()

  protected def produce(x: T) {
    coordinator ! Some(x)
    receive { case Next => }
  }

  private val producer: Actor = actor {
    receive {
      case Next =>
        produceValues()
        coordinator ! None
    }
  }

  private val coordinator: Actor = actor {
    loop {
      react {
        case Next =>
          producer ! Next
          reply {
            receive { case x: Option[_] => x }
          }
        case Stop => exit('stop)
      }
    }
  }

  def iterator = new Iterator[T] {
    private var current: Any = Undefined
    private def lookAhead = {
      if (current == Undefined) current = coordinator !? Next
      current
    }

    def hasNext: Boolean = lookAhead match {
      case Some(x) => true
      case None => { coordinator ! Stop; false }
    }

    def next(): T = lookAhead match {
      case Some(x) => current = Undefined; x.asInstanceOf[T]
    }
  }


}
