package ru.bsearchtree

import groovy.transform.Immutable
import org.junit.Test
import static ru.bsearchtree.BST5.Node.emptyNode
import static ru.bsearchtree.BST5.Node.node

/**
 * User: dima
 * Date: 22/11/2011
 */
class BST5 {
  @Test public void should_add_elements_keeping_its_structure() {
    assert emptyNode() == emptyNode()
    assert emptyNode().add(1) == node(1)

    assert node(1).add(2) == node(1, null, node(2))
    assert node(1).add(0) == node(1, node(0))

    assert node(1).add(0).add(2) == node(1, node(0), node(2))
    assert node(1).add(2).add(0) == node(1, node(0), node(2))
  }

  @Test public void should_determine_if_it_contains_an_element() {
    assert emptyNode().contains(0) == false

    def tree = node(1, node(0), node(2))
    assert tree.contains(0)
    assert [-1, 0, 1, 2, 3].collect { tree.contains(it) } == [false, true, true, true, false]
  }

  @Test public void should_perform_right_rotation_of_node() {
    assert emptyNode().rotateRight() == emptyNode()

    assert node(1).rotateRight() == node(1)
    assert node(1, null, node(2)).rotateRight() == node(1, null, node(2))
    assert node(1, node(0), null).rotateRight() == node(0, null, node(1))
    assert node(1, node(0), node(2)).rotateRight() == node(0, null, node(1, null, node(2)))

    assert node(3,
            node(1, node(0), node(2)),
            node(5, node(4), node(6)),
    ).rotateRight() == node(1,
            node(0),
            node(3, node(2), node(5, node(4), node(6)))
    )
  }

  @Test public void should_perform_left_rotation_of_node() {
    assert emptyNode().rotateLeft() == emptyNode()

    assert node(1).rotateLeft() == node(1)
    assert node(1, null, node(2)).rotateLeft() == node(2, node(1))
    assert node(1, node(0), null).rotateLeft() == node(1, node(0))
    assert node(1, node(0), node(2)).rotateLeft() == node(2, node(1, node(0)))

    assert node(3,
            node(1, node(0), node(2)),
            node(5, node(4), node(6)),
    ).rotateLeft() == node(5,
            node(3, node(1, node(0), node(2)), node(4)),
            node(6)
    )
  }

  @Test public void should_remove_elements_keeping_structure() {
    assert emptyNode().remove(1) == emptyNode()
    assert node(1).remove(1) == emptyNode()
  }

  @Immutable
  static class Node {
    static Node EMPTY_NODE = new Node(Integer.MIN_VALUE, null, null)

    int value
    Node left
    Node right

    static Node node(value, left = null, right = null) {
      new Node(value, left, right)
    }

    static Node emptyNode() {
      EMPTY_NODE
    }

    def contains(valueToFind) {
      if (this == EMPTY_NODE) {
        false
      } else if (valueToFind == value) {
        true
      } else if (valueToFind < value) {
        left != null ? left.contains(valueToFind) : false
      } else { //if (valueToFind > value) {
        right != null ? right.contains(valueToFind) : false
      }
    }

    def add(int value) {
      if (this == EMPTY_NODE) node(value)
      else addTo(this, node(value))
    }

    def rotateRight() {
      if (this == EMPTY_NODE || left == null) this
      else node(left.value, left.left, node(value, left.right, right))
    }

    def rotateLeft() {
      if (this == EMPTY_NODE || right == null) this
      else node(right.value, node(value, left, right.left), right.right)
    }

    def remove(int value) {
      if (this == EMPTY_NODE) this
      else this
    }

    private static Node addTo(Node thisNode, Node nodeToAdd) {
      if (thisNode == null) {
        nodeToAdd
      } else if (nodeToAdd.value >= thisNode.value) {
        node(thisNode.value, thisNode.left, addTo(thisNode.right, nodeToAdd))
      } else { //if (nodeToAdd.value < thisNode.value) {
        node(thisNode.value, addTo(thisNode.left, nodeToAdd), thisNode.right)
      }
    }

    @Override
    String toString() {
      "Node{${value}"+ ", left=" + left + ", right=" + right + '}';
    }
  }

}
