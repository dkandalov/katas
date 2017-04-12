package katas.groovy.tree

import org.junit.Test

/**
 * User: dima
 * Date: 29/1/11
 */
class Tree3 {
  @Test
  public void shouldTraverseTree() {
    GNode tree =
      new GNode("root",
            new GNode("1",
                    new GNode("11"),
                    new GNode("12")),
            new GNode("2"))

    def nodesVisited = []
    traverse(tree) { nodesVisited << it.s }

    assert nodesVisited == ["root", "1", "11", "12", "2"]
  }

  void traverse(GNode node, Closure closure) {
    if (node == null) return

    closure.call(node)
    traverse(node.left, closure)
    traverse(node.right, closure)
  }
}

class GNode {
  String s
  GNode left
  GNode right

  GNode(String s) {
    this(s, null, null)
  }

  GNode(String s, GNode left, GNode right) {
    this.s = s
    this.left = left
    this.right = right
  }

  public String toString() {
    return "{" + s + '}';
  }
}
