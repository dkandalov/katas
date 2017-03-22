package katas.java.tree;

import org.junit.Test;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Oct 1, 2010
 */
public class TreeTest2 {
    @Test
    public void preOrderTreeTraversal() {
        Node root =
            node("root",
                node("1"),
                node("2")
            );

        assertThat(preOrderTraverse(root), equalTo(Arrays.asList("root", "1", "2")));
    }

    @Test
    public void inOrderTraverse() {
        Node root =
            node("root",
                node("1"),
                node("2")
            );

        assertThat(inOrderTraverse(root), equalTo(Arrays.asList("1", "root", "2")));
    }

    @Test
    public void postOrderTraverse() {
        Node root =
            node("root",
                node("1"),
                node("2")
            );

        assertThat(postOrderTraverse(root), equalTo(Arrays.asList("1", "2", "root")));
    }

    private static List<String> postOrderTraverse(Node root) {
        List<String> result = new LinkedList<String>();
        doPostOrderTraverse(result, root);
        return result;
    }

    private static void doPostOrderTraverse(List<String> result, Node node) {
        if (node == null) return;

        doPostOrderTraverse(result, node.left);
        doPostOrderTraverse(result, node.right);
        result.add(node.value);
    }

    private static List<String> inOrderTraverse(Node root) {
        return doInOrderTraverse(new LinkedList<String>(), root);
    }

    private static List<String> doInOrderTraverse(LinkedList<String> result, Node node) {
        if (node == null) return result;

        doInOrderTraverse(result, node.left);
        result.add(node.value);
        doInOrderTraverse(result, node.right);

        return result;
    }

    private static List<String> preOrderTraverse(Node root) {
        return doPreOrderTraverse(new LinkedList<String>(), root);
    }

    private static List<String> doPreOrderTraverse(LinkedList<String> result, Node node) {
        if (node == null) return result;
        
        result.add(node.value);
        doPreOrderTraverse(result, node.left);
        doPreOrderTraverse(result, node.right);
        return result;
    }

    private static Node node(String value, Node left, Node right) {
        return new Node(value, left, right);
    }
    private static Node node(String value) {
        return new Node(value, null, null);
    }

    private static class Node {
        private final String value;
        private final Node left;
        private final Node right;

        public Node(String value, Node left, Node right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }

        @SuppressWarnings({"RedundantIfStatement"})
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Node node = (Node) o;

            if (left != null ? !left.equals(node.left) : node.left != null) return false;
            if (right != null ? !right.equals(node.right) : node.right != null) return false;
            if (value != null ? !value.equals(node.value) : node.value != null) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = value != null ? value.hashCode() : 0;
            result = 31 * result + (left != null ? left.hashCode() : 0);
            result = 31 * result + (right != null ? right.hashCode() : 0);
            return result;
        }

        @Override
        public String toString() {
            return "Node{" +
                    "value='" + value + '\'' +
                    ", left=" + left +
                    ", right=" + right +
                    '}';
        }
    }
}
