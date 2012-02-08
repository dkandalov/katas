package ru.bsearchtree

import org.junit.Test

/**
 * User: dima
 * Date: 07/02/2012
 */
class BST6 {
  @Test public void GIVEN_aSetOfKeyValuePair_SHOULD_addThemToBST_AND_retrieveValuesByKeys() {
    assert new BST().get(1) == null
    assert new BST([a: 1]).get("a") == 1

    new BST([a: 1, b: 2]).with {
      assert get("a") == 1
      assert get("b") == 2
      assert get("c") == null
    }
    new BST([a: 1, b: 2, c: 3]).with {
      assert get("a") == 1
      assert get("b") == 2
      assert get("c") == 3
      assert get("d") == null
    }
    def data = [('a'..'f'), (1..6)].transpose()
    data.permutations().each { List list ->
      data.each { entry ->
        assert new BST(list).get(entry[0]) == entry[1]
      }
    }
  }

  private static class BST {
    Node root

    BST(List list) {
      list.each { put(it[0], it[1]) }
    }

    BST(Map map = [:]) {
      map.each { put(it.key, it.value) }
    }

    def put(key, value) {
      root = doPut(key, value, root)
    }

    private Node doPut(key, value, Node node) {
      if (node == null) {
        node = new Node(key, value)
      } else if (key > node.key) {
        node.right = doPut(key, value, node.right)
      } else { // if (key <= node.key)
        node.left = doPut(key, value, node.left)
      }
      node
    }

    def get(key) {
      doGet(key, root)
    }

    private def doGet(key, Node node) {
      if (node == null) {
        null
      } else if (key == node.key) { // forgot this :(
        node.value
      } else if (key > node.key) {
        doGet(key, node.right)
      } else { // if (key <= node.key)
        doGet(key, node.left)
      }
    }
  }

  private static class Node {
    Node left
    Node right
    def key
    def value

    Node(key, value) {
      this.key = key
      this.value = value
    }
  }
}
