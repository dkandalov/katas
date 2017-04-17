package katas.java.tree;

import katas.java.tree.common.Node;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static katas.java.tree.common.Node.node;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * https://www.hackerrank.com/challenges/is-binary-search-tree
 */
public class IsBST {
    boolean checkBST(Node<Integer> root) {
        if (root == null) return false;
        
        List<Node<Integer>> queue = new ArrayList<>();
        List<MinMax> minMaxQueue = new ArrayList<>();
        queue.add(root);
        minMaxQueue.add(new MinMax(Integer.MIN_VALUE, Integer.MAX_VALUE));

        while (!queue.isEmpty()) {
            Node<Integer> node = queue.remove(0);
            MinMax minMax = minMaxQueue.remove(0);

            if (node.data >= minMax.max) return false;
            if (node.data <= minMax.min) return false;

            if (node.left != null) {
                queue.add(node.left);
                minMaxQueue.add(new MinMax(minMax.min, node.data));
            }
            if (node.right != null) {
                queue.add(node.right);
                minMaxQueue.add(new MinMax(node.data, minMax.max));
            }
        }
        return true;
    }

    static class MinMax {
        final int min;
        final int max;

        MinMax(int min, int max) {
            this.min = min;
            this.max = max;
        }
    }

    @Test public void determineIfTreeIsBinarySearchTree() {
        assertThat(checkBST(node(3)), equalTo(true));
        assertThat(checkBST(node(3, node(2), node(4))), equalTo(true));
        assertThat(checkBST(node(3, node(4), node(2))), equalTo(false));
        assertThat(checkBST(node(3, node(3), node(3))), equalTo(false));

        assertThat(checkBST(
            node(3,
                node(2, node(1), node(5)),
                node(5)
        )), equalTo(false));
    }

    @Test public void longTree() {
        Node<Integer> tree = new Node<>(40_000);
        for (int i = tree.data - 1; i > 0; i--) {
            tree = new Node<>(i, null, tree);
        }
        assertThat(checkBST(tree), equalTo(true));

        tree = new Node<>(0);
        for (int i = tree.data + 1; i <= 40_000; i++) {
            tree = new Node<>(i, tree, null);
        }
        assertThat(checkBST(tree), equalTo(true));
    }
}
