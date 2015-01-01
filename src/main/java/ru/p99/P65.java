package ru.p99;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class P65 {
/*
    @Test public void assigningTreesWithDifferentTypes() {
        Tree<? extends Number> numberTree = new Node<>(1);
        Tree<Integer> integerTree = new Node<>(2);
        numberTree = integerTree;
    }
*/

    @Test public void treeLayout() {
        Tree<Character> tree =
                new Node<>('a',
                        new Node<>('b', new End<>(), new Node<>('c')),
                        new Node<>('d')
                );
        Tree<Character> layedOutTree = tree.layoutBinaryTree();
        assertThat(layedOutTree, equalTo(
                new PositionedNode<>(3, 1, new Node<>('a',
                        new PositionedNode<>(1, 2, new Node<>('b', new End<>(), new PositionedNode<>(2, 3, new Node<>('c')))),
                        new PositionedNode<>(4, 2, new Node<>('d'))
                )))
        );
    }

    private interface Tree<T> {
        Tree<T> layoutBinaryTree();

        @Override public String toString();
    }

    private static class Node<T> implements Tree<T> {
        public final T value;
        public final Tree<T> left;
        public final Tree<T> right;

        public Node(T value) {
            this(value, new End<>(), new End<>());
        }

        public Node(T value, Tree<T> left, Tree<T> right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }

        @Override public PositionedNode<T> layoutBinaryTree() {
            return null;
        }

        @Override public String toString() {
            return "T(" + value + " " + left.toString() + " " + right.toString() + " )";
        }
    }

    private static class PositionedNode<T> extends Node<T> {
        public final int x;
        public final int y;

        public PositionedNode(int x, int y, Node<T> node) {
            super(node.value, node.left, node.right);
            this.x = x;
            this.y = y;
        }

        @Override public String toString() {
            return "T[" + x + "," + y + "](" + value + " " + left.toString() + " " + right.toString() + " )";
        }
    }

    private static class End<T> implements Tree<T> {
        @Override public End<T> layoutBinaryTree() {
            return this;
        }

        @Override public String toString() {
            return ".";
        }
    }
}
