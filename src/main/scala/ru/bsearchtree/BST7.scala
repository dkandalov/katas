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
    new BST().get(1) should equal(None)
    new BST().put(1, "a").get(1) should equal(Some("a"))

    var bst = new BST().putAll(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    bst.get(0) should equal(None)
    bst.get(1) should equal(Some("a"))
    bst.get(2) should equal(Some("b"))
    bst.get(3) should equal(Some("c"))
    bst.get(4) should equal(None)

    Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d").toList.permutations.foreach { list =>
      val bst = new BST().putAll(list)
      bst.get(0) should equal(None)
      bst.get(1) should equal(Some("a"))
      bst.get(2) should equal(Some("b"))
      bst.get(3) should equal(Some("c"))
      bst.get(4) should equal(Some("d"))
      bst.get(5) should equal(None)
    }
  }

  private class BST(val root: Node = null) {

    def putAll(map: Map[Int, String]): BST = {
      putAll(map.toList)
    }

    @tailrec final def putAll(list: List[(Int, String)]): BST = {
      if (list.isEmpty) return this
      put(list.head._1, list.head._2).putAll(list.tail)
    }

    def put(key: Int, value: String): BST = {
      new BST(doPut(key, value, root))
    }

    private def doPut(key: Int, value: String, node: Node): Node = {
      if (node == null) Node(key, value)
      else if (key > node.key) node.withRight(doPut(key, value, node.right))
      else node.withLeft(doPut(key, value, node.left))
    }

    def get(key: Int): Option[String] = {
      doGet(key, root)
    }

    @tailrec private def doGet(key: Int, node: Node): Option[String] = {
      if (node == null) None
      else if (key == node.key) Some(node.value)
      else if (key < node.key) doGet(key, node.left)
      else doGet(key, node.right)
    }
  }

  private case class Node(key: Int, value: String, left: Node = null, right: Node = null) {
    def withLeft(node: Node) = Node(key, value, node, right)
    def withRight(node: Node) = Node(key, value, left, node)
  }
}