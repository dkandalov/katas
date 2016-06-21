package hackerank;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertThat;

public class SelfBalancingTree {
    @Test public void treeHeight() {
        Node tree =
            node('a',
                node('b'),
                node('c',
                    node('d'),
                    node('e')));

        assertThat(height(tree), equalTo(3));
        assertThat(height(tree.left), equalTo(1));
        assertThat(height(tree.right), equalTo(2));
    }

    @Test public void insertingValue() {
        Node tree =
            node(3,
                node(1),
                node(5));

        assertThat(insert(tree, 4), equalTo(
            node(3,
                node(1),
                node(5,
                    node(4),
                    null))
        ));
    }

    @Test public void heightAfterInsertion() {
        Node tree = node(5);
        assertThat(tree.ht, equalTo(1));

        insert(tree, node(4));
        assertThat(tree.ht, equalTo(2));

        insert(tree, node(6));
        assertThat(tree.ht, equalTo(2));

        insert(tree, node(7));
        assertThat(tree.ht, equalTo(3));

        insert(tree, node(8));
        assertThat(tree.ht, equalTo(4));
    }

    @Test public void rotateRight() {
        Node tree =
                node(5,
                        node(4,
                                node(3), null),
                        null);

        assertThat(rotateRight(tree.left, tree), equalTo(
                node(4,
                        node(3),
                        node(5))
        ));
    }

    @Test public void heightAfterBalancedInsertion() {
        Node tree = node(5);
        assertThat(tree.ht, equalTo(1));

        tree = insertAndBalance(tree, node(6));
        assertThat(tree.ht, equalTo(2));

        tree = insertAndBalance(tree, node(7));
        assertThat(tree.ht, equalTo(2));

        tree = insertAndBalance(tree, node(8));
        assertThat(tree.ht, equalTo(3));

        tree = insertAndBalance(tree, node(9));
        assertThat(tree.ht, equalTo(3));
    }

    @Test public void exampleOfBalancingFactor() {
        Node node = node(3);
        node = insert(node, 2); System.out.println(balanceFactorOf(node));
        node = insert(node, 4); System.out.println(balanceFactorOf(node));
        node = insert(node, 5); System.out.println(balanceFactorOf(node));
        node = insert(node, 6); System.out.println(balanceFactorOf(node));
        System.out.println(node);

        assertThat(balanceFactorOf(node), lessThan(2));
    }

    private static Node insert(Node node, int value) {
        return insertAndBalance(node, node(value));
    }

    private static Node insertAndBalance(Node node, Node newNode) {
        return balance(insert(node, newNode));
    }

    private static Node insert(Node node, Node newNode) {
        if (node == null) return newNode;
        if (newNode.val <= node.val) {
            node.left = insert(node.left, newNode);
        } else {
            node.right = insert(node.right, newNode);
        }

        updateHeight(node);

        return node;
    }

    private static Node balance(Node node) {
        int leftHeight = heightOf(node.left);
        int leftLeftHeight = leftHeight == 0 ? 0 : heightOf(node.left.left);
        int leftRightHeight = leftHeight == 0 ? 0 : heightOf(node.left.right);

        int rightHeight = heightOf(node.right);
        int rightLeftHeight = rightHeight == 0 ? 0 : heightOf(node.right.left);
        int rightRightHeight = rightHeight == 0 ? 0 : heightOf(node.right.right);

        if (Math.abs(leftHeight - rightHeight) <= 1) return node;

        if (leftHeight > rightHeight) {
            if (leftRightHeight > leftLeftHeight) {
                rotateLeft(node.left.right, node);
            }
            return rotateRight(node.left, node);
        } else {
            if (rightLeftHeight > rightRightHeight) {
                rotateRight(node.right.left, node);
            }
            return rotateLeft(node.right, node);
        }
    }

    private static void updateHeight(Node node) {
        node.ht = 1 + Math.max(heightOf(node.left), heightOf(node.right));
    }

    private static Node rotateRight(Node node, Node parentNode) {
        parentNode.left = node.right;
        node.right = parentNode;

        updateHeight(parentNode);
        updateHeight(node);

        return node;
    }

    private static Node rotateLeft(Node node, Node parentNode) {
        parentNode.right = node.left;
        node.left = parentNode;

        updateHeight(parentNode);
        updateHeight(node);

        return node;
    }

    private static int balanceFactorOf(Node node) {
        return heightOf(node.left) - heightOf(node.right);
    }

    private static int heightOf(Node node) {
        return node == null ? 0 : node.ht;
    }

    private static Node node(int value) {
        return node(value, null, null);
    }

    private static Node node(int value, Node left, Node right) {
        Node node = new Node();
        node.val = value;
        node.left = left;
        node.right = right;
        node.ht = 1;
        return node;
    }

    private static int height(Node node) {
        if (node == null) {
            return 0;
        } else {
            return 1 + Math.max(height(node.left), height(node.right));
        }
    }

    private static class Node {
        public int val;
        public Node left;
        public Node right;
        public int ht;

        @Override public String toString() {
            String s = "{val:" + val;
            if (left != null || right != null) {
                s += ",left:" + left;
                s += ",right:" + right;
            }
            return s + '}';
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Node node = (Node) o;

            if (val != node.val) return false;
            if (left != null ? !left.equals(node.left) : node.left != null) return false;
            return right != null ? right.equals(node.right) : node.right == null;
        }

        @Override public int hashCode() {
            int result = val;
            result = 31 * result + (left != null ? left.hashCode() : 0);
            result = 31 * result + (right != null ? right.hashCode() : 0);
            return result;
        }
    }
}
