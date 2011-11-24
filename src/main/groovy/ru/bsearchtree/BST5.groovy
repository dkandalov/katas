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
  @Test public void given_BST_should_add_elements_to_it_keeping_its_structure() {
    assert emptyNode() == emptyNode()
    assert emptyNode().add(1) == node(1)

    assert node(1).add(2) == node(1, null, node(2))
    assert node(1).add(0) == node(1, node(0))

    assert node(1).add(0).add(2) == node(1, node(0), node(2))
    assert node(1).add(2).add(0) == node(1, node(0), node(2))
  }

  @Test public void given_BST_should_determine_if_it_contains_an_element() {
    def tree = node(1, node(0), node(2))
    assert [-1, 0, 1, 2, 3].collect { tree.contains(it) } == [false, true, true, true, false]
  }

  @Test public void given_BST_should_remove_elements_from_it_keeping_its_structure() {
//    assert emptyNode().remove(1) == emptyNode()
//    assert node(1).remove(1) == emptyNode()
  }

  @Immutable
  static class Node {
    static Node EMPTY_NODE = new Node(0, null, null)

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
      if (valueToFind == value) {
        true
      } else if (valueToFind < value) {
        left != null ? left.contains(valueToFind) : false
      } else { //if (valueToFind < value) {
        right != null ? right.contains(valueToFind) : false
      }
    }

    def add(int value) {
      if (this == EMPTY_NODE) node(value)
      else addTo(this, node(value))
    }

    def remove(int value) {
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
      "Node{" +
              "value=" + value +
              ", left=" + left +
              ", right=" + right +
              '}';
    }
  }

}
