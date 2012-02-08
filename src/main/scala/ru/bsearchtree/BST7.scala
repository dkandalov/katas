package ru.bsearchtree

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import annotation.tailrec

/**
 * User: dima
 * Date: 08/02/2012
 */
class BST7 extends ShouldMatchers {
  @Test def GIVEN_aSetOfKeysAndValues_SHOULD_addThemToBST_AND_determineIfBSTContainsThem() {
    new BST[Int, String]().get(1) should equal(None)
    new BST[Int, String]().put(1, "a").get(1) should equal(Some("a"))
    new BST[Int, Int]().put(1, 123).get(1) should equal(Some(123))

    val bst = new BST[Int, String]().putAll(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    bst.get(0) should equal(None)
    bst.get(1) should equal(Some("a"))
    bst.get(2) should equal(Some("b"))
    bst.get(3) should equal(Some("c"))
    bst.get(4) should equal(None)

    Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d").toList.permutations.foreach { list =>
      val bst = new BST[Int, String]().putAll(list)
      bst.get(0) should equal(None)
      bst.get(1) should equal(Some("a"))
      bst.get(2) should equal(Some("b"))
      bst.get(3) should equal(Some("c"))
      bst.get(4) should equal(Some("d"))
      bst.get(5) should equal(None)
    }
  }

  private class BST[K, V](val root: Node[K, V] = null) {

    def putAll(map: Map[K, V])(implicit orderer: K => Ordered[K]): BST[K, V] = {
      putAll(map.toList)
    }

    @tailrec final def putAll(list: List[(K, V)])(implicit orderer: K => Ordered[K]): BST[K, V] = {
      if (list.isEmpty) return this
      put(list.head._1, list.head._2).putAll(list.tail)
    }

    def put(key: K, value: V)(implicit orderer: K => Ordered[K]): BST[K, V] = {
      new BST(doPut(key, value, root))
    }

    private def doPut(key: K, value: V, node: Node[K, V])(implicit orderer: K => Ordered[K]): Node[K, V] = {
      if (node == null) Node(key, value)
      else if (key > node.key) node.withRight(doPut(key, value, node.right))
      else node.withLeft(doPut(key, value, node.left))
    }

    def get(key: K)(implicit orderer: K => Ordered[K]): Option[V] = {
      doGet(key, root)
    }

    @tailrec private def doGet(key: K, node: Node[K, V])(implicit orderer: K => Ordered[K]): Option[V] = {
      if (node == null) None
      else if (key == node.key) Some(node.value)
      else if (key < node.key) doGet(key, node.left)
      else doGet(key, node.right)
    }

  }

  private case class Node[K, V](key: K, value: V, left: Node[K, V] = null, right: Node[K, V] = null) {
    def withLeft(node: Node[K, V]) = Node(key, value, node, right)
    def withRight(node: Node[K, V]) = Node(key, value, left, node)
  }
}