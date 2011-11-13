package ru.bsearchtree

import org.junit.Test
import static ru.bsearchtree.Node4.node

/**
 * User: dima
 * Date: 13/11/2011
 */
class BST4 {
  @Test public void shouldAddNodesToTreeKeepingItsProperties() {
    assert node() == node()
    assert node(2) == node(2)
    assert node().add(node(2)) == node(2)
    assert node(2).add(node()) == node(2)
    assert node(2).add(node(1)) == node(2, node(1))
    assert node(2).add(node(1)).add(node(3)) == node(2, node(1), node(3))
    assert node(2).add(node(1)).add(node(3)).add(node(4)) == node(2, node(1), node(3, null, node(4)))
    assert node(2).add(node(1)).add(node(3)).add(node(0)) == node(2, node(1, node(0)), node(3))
  }

  @Test public void shouldSortByInOrderTraversal() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([1, 2]) == [1, 2]
    assert sort([2, 1]) == [1, 2]
    assert sort([3, 1, 2]) == [1, 2, 3]

    [1, 2, 3, 4, 5].permutations().each { list ->
      assert sort(list) == [1, 2, 3, 4, 5]
    }
  }

  def sort(List list) {
    list.inject(node()) { Node4 bst, int value -> bst.add(node(value)) }.traverseInOrder()
  }
}

@Immutable
final class Node4 {
  static NO_VALUE = Integer.MIN_VALUE

  int value
  Node4 left, right

  static Node4 node(value = NO_VALUE, left = null, right = null) {
    new Node4(value, left, right)
  }

  Node4 add(Node4 node) {
    if (node.value == NO_VALUE) return this
    else if (this.value == NO_VALUE) return node
    else add(this, node)
  }

  private Node4 add(Node4 thisNode, Node4 nodeToAdd) {
    if (thisNode == null) nodeToAdd
    else if (nodeToAdd.value <= thisNode.value) node(thisNode.value, add(thisNode.left, nodeToAdd), thisNode.right)
    else if (nodeToAdd.value > thisNode.value) node(thisNode.value, thisNode.left, add(thisNode.right, nodeToAdd))
    else throw new IllegalStateException()
  }

  List traverseInOrder() {
    if (this.value == NO_VALUE) return []
    else traverseInOrder(this)
  }

  private List traverseInOrder(Node4 thisNode) {
    if (thisNode == null) return []
    else [traverseInOrder(thisNode.left), thisNode.value, traverseInOrder(thisNode.right)].flatten() // forgot flatten()
  }

  @Override
  String toString() {
    "Node(${value}${left != null ? ', ' + left.toString() : ''}${right != null ? ', ' + right.toString() : ''})"
  }
}