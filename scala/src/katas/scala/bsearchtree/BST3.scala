package katas.scala.bsearchtree

import org.junit.Test
import org.scalatest.Matchers

/**
 * User: dima
 * Date: 04/11/2011
 */
class BST3 extends Matchers {
  @Test def shouldFindIfBinarySearchTreeContainsElement() {
    var bst = BST()
    bst.contains(1) should equal(false)

    bst = bst.add(1)
    bst.contains(0) should equal(false)
    bst.contains(1) should equal(true)
    bst.contains(2) should equal(false)

    bst = bst.add(2)
    bst.contains(0) should equal(false)
    bst.contains(1) should equal(true)
    bst.contains(2) should equal(true)
    bst.contains(3) should equal(false)

    bst = bst.add(3)
    bst.contains(0) should equal(false)
    bst.contains(1) should equal(true)
    bst.contains(2) should equal(true)
    bst.contains(3) should equal(true)
    bst.contains(4) should equal(false)
  }

  @Test def shouldAddElementsToBottomOfBST() {
    BST() should equal(BST())
    BST(1) should equal(BST(1))
    BST().add(1) should equal(BST(1))

    BST(1).add(2) should equal(BST(1, null, BST(2)))
    BST(2).add(1) should equal(BST(2, BST(1)))

    BST(1).add(2).add(3) should equal(BST(1, null, BST(2, null, BST(3))))
    BST(3).add(2).add(1) should equal(BST(3, BST(2, BST(1))))
    BST(2).add(3).add(1) should equal(BST(2, BST(1), BST(3)))
    BST(2).add(1).add(3) should equal(BST(2, BST(1), BST(3)))

    BST(3).add(2).add(1).add(4) should equal(BST(3, BST(2, BST(1)), BST(4)))
  }

  case class BST(value: Int = Int.MinValue, left: BST = null, right: BST = null) {
    def add(n: Int): BST = {
      add(this, n)
    }

    private def add(bst: BST, n: Int): BST = {
      if (bst == null || bst.value == Int.MinValue) BST(n)
      else if (n <= bst.value) BST(bst.value, add(bst.left, n), bst.right)
      else if (n > bst.value) BST(bst.value, bst.left, add(bst.right, n))
      else throw new IllegalStateException()
    }

    def contains(n: Int): Boolean = {
      contains(this, n)
    }

    private def contains(bst: BST, n: Int): Boolean = {
      if (bst == null) false
      else if (n == bst.value) true
      else if (n < bst.value) contains(bst.left, n)
      else if (n > bst.value) contains(bst.right, n)
      else throw new IllegalStateException()
    }
  }

}